(** Agent framework: match-driven decisioning and auto-tool loops *)

module Context = struct
  type t = {
    sw : Eio.Switch.t;
    env : Eio_unix.Stdenv.base;
    client : Client.t;
    clock : float Eio.Time.clock_ty Eio.Resource.t;
  }

  let make ~sw ~env ~client =
    { sw; env; client; clock = Eio.Stdenv.clock env }
end

module Budget = struct
  type t = {
    max_steps : int;
    max_tool_rounds : int;
    max_total_tool_calls : int;
    max_elapsed_s : float option;
  }

  let default =
    {
      max_steps = 20;
      max_tool_rounds = 10;
      max_total_tool_calls = 50;
      max_elapsed_s = None;
    }
end

module Error = struct
  type framework =
    | Api of Errors.t
    | Decision_parse of string
    | Tool_parse of string
    | Tool_handler of string
    | Budget_exhausted of [ `Steps | `Tool_rounds | `Tool_calls | `Elapsed ]
    | Protocol of string

  type 'e t = Framework of framework | User of 'e
end

module Observation = struct
  type t =
    | Content of string
    | Tool_calls of Tool.tool_call list
    | Content_and_tool_calls of string * Tool.tool_call list
    | Refusal of string
    | Empty

  let of_response (response : Chat.response) =
    match response.choices with
    | [] -> Empty
    | choice :: _ ->
        let msg = choice.message in
        (match msg.refusal with
         | Some r -> Refusal r
         | None ->
             let tool_calls =
               match msg.tool_calls with
               | Some [] -> None
               | other -> other
             in
             let content =
               match msg.content with
               | Some (Content.String_content s) -> Some s
               | _ -> None
             in
             match content, tool_calls with
             | Some c, Some calls -> Content_and_tool_calls (c, calls)
             | Some c, None -> Content c
             | None, Some calls -> Tool_calls calls
             | None, None -> Empty)
end

module Decision = struct
  type t = { action : string; args : Yojson.Safe.t }

  type parse_error =
    | No_call
    | Multiple_calls
    | Wrong_tool of string
    | Invalid_args of string

  let default_args_schema =
    `Assoc [ ("type", `String "object"); ("additionalProperties", `Bool true) ]

  let tool ?(name = "route") ?(strict = false) ~actions ?args_schema () =
    let action_schema =
      `Assoc [
        ("type", `String "string");
        ("enum", `List (List.map (fun a -> `String a) actions));
      ]
    in
    let args_schema = Option.value args_schema ~default:default_args_schema in
    let schema =
      `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("action", action_schema);
          ("args", args_schema);
        ]);
        ("required", `List [ `String "action"; `String "args" ]);
        ("additionalProperties", `Bool false);
      ]
    in
    let parse json =
      let open Yojson.Safe.Util in
      let action = json |> member "action" |> to_string in
      let args = json |> member "args" in
      { action; args }
    in
    Typed_tool.create ~name ~schema ~parse ~strict ()

  let parse_tool_calls (tool : t Typed_tool.t) (calls : Tool.tool_call list) =
    match calls with
    | [] -> Error No_call
    | _ :: _ :: _ -> Error Multiple_calls
    | [call] ->
        if call.function_.name <> tool.name then Error (Wrong_tool call.function_.name)
        else
          (try
             let json = Yojson.Safe.from_string call.function_.arguments in
             let open Yojson.Safe.Util in
             let action = json |> member "action" |> to_string in
             let args = json |> member "args" in
             Ok { action; args }
           with exn ->
             Error (Invalid_args (Printexc.to_string exn)))

  let decode f decision =
    match f decision.action decision.args with
    | Ok v -> Ok v
    | Error msg -> Error (Invalid_args msg)
end

module Tools = struct
  type tool_error = { tool : string; message : string }

  type ('ctx,'params) handler =
    'ctx -> 'params -> (string, tool_error) result

  type 'ctx handler_fn =
    'ctx -> Tool.tool_call -> (string, tool_error) result option

  type 'ctx t = {
    tools : Tool.tool list;
    handlers : 'ctx handler_fn list;
  }

  let empty = { tools = []; handlers = [] }

  let add (typed_tool : 'params Typed_tool.t) (handle : ('ctx,'params) handler) (ts : 'ctx t) =
    let raw_tool = Typed_tool.to_tool typed_tool in
    let handler ctx (call : Tool.tool_call) =
      if Typed_tool.matches typed_tool call then
        match Typed_tool.parse_call typed_tool call with
        | Some params -> Some (handle ctx params)
        | None ->
            Some (Error { tool = call.function_.name; message = "parse error: failed to parse tool arguments" })
      else None
    in
    { tools = ts.tools @ [raw_tool]; handlers = ts.handlers @ [handler] }

  let to_tools ts = ts.tools

  type policy = {
    on_unhandled : [ `Fail | `Ignore | `Message ];
    on_error : [ `Fail | `Message ];
    error_to_string : tool_error -> string;
  }

  let default_policy = {
    on_unhandled = `Fail;
    on_error = `Fail;
    error_to_string = (fun e -> e.message);
  }

  let handle_all ?(policy = default_policy) ts ctx (calls : Tool.tool_call list) =
    let rec try_handlers call = function
      | [] -> None
      | h :: rest ->
          (match h ctx call with
           | None -> try_handlers call rest
           | Some result -> Some result)
    in
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | call :: rest ->
          (match try_handlers call ts.handlers with
           | None ->
              let err = { tool = call.function_.name; message = "unhandled tool: " ^ call.function_.name } in
              (match policy.on_unhandled with
                | `Ignore -> loop acc rest
                | `Message ->
                    let msg = Message.tool ~tool_call_id:call.id (policy.error_to_string err) in
                    loop (msg :: acc) rest
                | `Fail -> Error err)
           | Some (Ok result) ->
               let msg = Message.tool ~tool_call_id:call.id result in
               loop (msg :: acc) rest
           | Some (Error err) ->
               (match policy.on_error with
                | `Message ->
                    let msg = Message.tool ~tool_call_id:call.id (policy.error_to_string err) in
                    loop (msg :: acc) rest
                | `Fail -> Error err))
    in
    loop [] calls
end

module Loop = struct
  type ('s,'r,'e) outcome =
    | Continue of 's
    | Done of 'r
    | Fail of 'e Error.t

  let run ~ctx ~budget ~step ~init =
    let start = Eio.Time.now ctx.Context.clock in
    let rec loop steps state =
      if steps >= budget.Budget.max_steps then
        Error (Error.Framework (Error.Budget_exhausted `Steps))
      else
        match budget.Budget.max_elapsed_s with
        | Some max_elapsed when (Eio.Time.now ctx.Context.clock -. start) > max_elapsed ->
            Error (Error.Framework (Error.Budget_exhausted `Elapsed))
        | _ ->
            (match step state with
             | Continue next -> loop (steps + 1) next
             | Done r -> Ok r
             | Fail e -> Error e)
    in
    loop 0 init
end

module Auto = struct
  type model_call =
    ctx:Context.t ->
    tools:Tool.tool list ->
    messages:Message.t list ->
    (Chat.response, Errors.t) result

  type result = {
    response : Chat.response;
    messages : Message.t list;
  }

  let response_message (response : Chat.response) =
    match response.choices with
    | [] -> None
    | choice :: _ -> Some choice.message

  let run ~(ctx:Context.t) ~budget ~tools ~model ~messages ?(policy = Tools.default_policy) ?model_call () =
    let start = Eio.Time.now ctx.Context.clock in
    let tools_list = Tools.to_tools tools in
    let default_model_call ~ctx ~tools ~messages =
      let tools_opt = if tools = [] then None else Some tools in
      Api_chat.send ~sw:ctx.Context.sw ~env:ctx.Context.env ctx.Context.client
        ~model ?tools:tools_opt ~messages ()
    in
    let call_model = Option.value model_call ~default:default_model_call in
    let rec loop steps tool_rounds total_tool_calls messages =
      if steps >= budget.Budget.max_steps then
        Error (Error.Budget_exhausted `Steps)
      else
        match budget.Budget.max_elapsed_s with
        | Some max_elapsed when (Eio.Time.now ctx.Context.clock -. start) > max_elapsed ->
            Error (Error.Budget_exhausted `Elapsed)
        | _ ->
            match call_model ~ctx ~tools:tools_list ~messages with
            | Error e -> Error (Error.Api e)
            | Ok response ->
                (match response_message response with
                 | None -> Error (Error.Protocol "No message in response")
                 | Some msg ->
                     let observation = Observation.of_response response in
                     (match observation with
                      | Observation.Tool_calls calls
                      | Observation.Content_and_tool_calls (_, calls) ->
                          if tool_rounds + 1 > budget.Budget.max_tool_rounds then
                            Error (Error.Budget_exhausted `Tool_rounds)
                          else
                            let call_count = List.length calls in
                            if total_tool_calls + call_count > budget.Budget.max_total_tool_calls then
                              Error (Error.Budget_exhausted `Tool_calls)
                            else
                              (match Tools.handle_all ~policy tools ctx calls with
                               | Ok tool_messages ->
                                   let messages = messages @ (msg :: tool_messages) in
                                   loop (steps + 1) (tool_rounds + 1) (total_tool_calls + call_count) messages
                               | Error err ->
                                   if String.starts_with ~prefix:"parse error:" err.message
                                      || String.starts_with ~prefix:"unhandled tool:" err.message
                                   then Error (Error.Tool_parse err.message)
                                   else Error (Error.Tool_handler err.message))
                      | _ ->
                          let messages = messages @ [msg] in
                          Ok { response; messages }))
    in
    loop 0 0 0 messages
end
