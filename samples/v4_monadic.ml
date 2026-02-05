(** Version 4: Monadic / Let Operators

    Using let* / let+ for sequencing agent actions.
    Familiar to FP users, clean sequential flow.
*)

(* NOTE: This is pseudocode/design sketch, not compilable *)

open Openrouter

(* ============================================================
   Types
   ============================================================ *)

type image = string

type action_params = {
  action : string;
  velocity : float option;
  duration : float option;
  expected_outcome : string;
}

type tool_call_result =
  | Task_complete of string
  | Execute_action of action_params
  | No_tool

(* ============================================================
   Robot Interface (stubbed)
   ============================================================ *)

module Robot = struct
  type t = unit

  let capture_image _robot : image = "(base64 image data)"

  let execute _robot action _params =
    Printf.printf "[EXECUTING]: %s\n" action;
    "Command sent to robot"
end

(* ============================================================
   Agent Monad DSL (hypothetical)
   ============================================================ *)

module Agent = struct
  module Monad = struct
    (* The agent monad carries conversation state *)
    type 'a t = {
      run : state -> (state * 'a, error) result
    }

    and state = {
      messages : Message.t list;
      system_prompt : string option;
      model : string;
      client : Client.t;
      sw : Eio.Switch.t;
      env : Eio_unix.Stdenv.base;
    }

    and error = [ `Api_error of Errors.error | `Max_iterations | `No_content ]

    (* Monad operations *)
    let return x = { run = fun s -> Ok (s, x) }

    let bind m f = {
      run = fun s ->
        match m.run s with
        | Error e -> Error e
        | Ok (s', a) -> (f a).run s'
    }

    let ( let* ) = bind
    let ( let+ ) m f = bind m (fun x -> return (f x))

    (* Agent primitives *)
    let set_system prompt = {
      run = fun s -> Ok ({ s with system_prompt = Some prompt }, ())
    }

    let set_model model = {
      run = fun s -> Ok ({ s with model }, ())
    }

    let get_messages = {
      run = fun s -> Ok (s, s.messages)
    }

    let add_message msg = {
      run = fun s -> Ok ({ s with messages = s.messages @ [msg] }, ())
    }

    (* Send a message and get response *)
    let send_user content ~tools = {
      run = fun s ->
        let messages = s.messages @ [Message.user content] in
        let result =
          Pipeline.messages messages
          |> Pipeline.model s.model
          |> Pipeline.tools tools
          |> Pipeline.run ~sw:s.sw ~env:s.env s.client
        in
        match result with
        | Error e -> Error (`Api_error e)
        | Ok response ->
            let assistant_msg = (* extract from response *) Message.assistant "" in
            let messages = messages @ [assistant_msg] in
            Ok ({ s with messages }, response)
    }

    let send_user_with_image content image ~tools = {
      run = fun s ->
        let msg = Message.user_with_parts [
          Content.text_part content;
          Content.image_base64 ~media_type:"image/jpeg" image;
        ] in
        let messages = s.messages @ [msg] in
        let result =
          Pipeline.messages messages
          |> Pipeline.model s.model
          |> Pipeline.tools tools
          |> Pipeline.run ~sw:s.sw ~env:s.env s.client
        in
        match result with
        | Error e -> Error (`Api_error e)
        | Ok response ->
            let assistant_msg = Message.assistant "" in
            let messages = messages @ [assistant_msg] in
            Ok ({ s with messages }, response)
    }

    let send_tool_result ~tool_call_id result = {
      run = fun s ->
        let msg = Message.tool ~tool_call_id result in
        Ok ({ s with messages = s.messages @ [msg] }, ())
    }

    (* Run the agent monad *)
    let run ~sw ~env client agent =
      let initial_state = {
        messages = [];
        system_prompt = None;
        model = "gpt-4";
        client;
        sw;
        env;
      } in
      match agent.run initial_state with
      | Ok (_, result) -> result
      | Error e -> Error e
  end
end

(* ============================================================
   Prompts
   ============================================================ *)

let system_prompt = {|You are controlling a Unitree Go2 quadruped robot.

## AVAILABLE ACTIONS
- stand_up, stand_down, move_forward, move_backward
- turn_left, turn_right, stop, recovery_stand

## PHYSICS
- distance = velocity × duration
- rotation = angular_velocity × duration

Call task_complete when done.|}

let think_prompt ~task =
  Printf.sprintf "TASK: %s\n\nThis is the current view. Think about what to do." task

let reflect_prompt ~action ~prediction =
  Printf.sprintf
    {|This is the view AFTER your action: %s
Your prediction was: "%s"

Did it match? What should you do next?|}
    action prediction

let format_action params =
  Printf.sprintf "%s(d=%g)" params.action (Option.value params.duration ~default:1.0)

(* ============================================================
   Parsing helpers (stubbed)
   ============================================================ *)

let parse_tool_call _response : tool_call_result = No_tool
let get_tool_call_id _response = "call_123"

(* ============================================================
   The Robot Agent with Monadic Style
   ============================================================ *)

let robot_agent ~robot task =
  let open Agent.Monad in

  (* Set up the agent *)
  let* () = set_system system_prompt in
  let* () = set_model "anthropic/claude-sonnet-4" in

  (* Recursive agent loop *)
  let rec loop ~image ~last_action ~last_prediction ~fuel =
    if fuel <= 0 then return (Error `Max_iterations) else

    (* Phase 1: Think *)
    let think_content = match last_action with
      | None -> think_prompt ~task
      | Some action -> reflect_prompt ~action ~prediction:(Option.value last_prediction ~default:"")
    in

    let* _thinking_response =
      send_user_with_image think_content image ~tools:[]
    in

    (* Phase 2: Request action *)
    let* response =
      send_user "Now call execute_action or task_complete."
        ~tools:[]  (* actual tools would go here *)
    in

    (* Phase 3: Handle tool calls *)
    match parse_tool_call response with
    | Task_complete summary ->
        let* () = send_tool_result ~tool_call_id:(get_tool_call_id response) "TASK_COMPLETE" in
        return (Ok summary)

    | Execute_action params ->
        (* Execute the action *)
        let result = Robot.execute robot params.action params in
        let* () = send_tool_result ~tool_call_id:(get_tool_call_id response) result in

        (* Capture new image *)
        let new_image = Robot.capture_image robot in

        (* Recurse with updated state *)
        loop
          ~image:new_image
          ~last_action:(Some (format_action params))
          ~last_prediction:(Some params.expected_outcome)
          ~fuel:(fuel - 1)

    | No_tool ->
        (* No tool call, retry *)
        loop ~image ~last_action ~last_prediction ~fuel:(fuel - 1)
  in

  (* Start the loop *)
  let initial_image = Robot.capture_image robot in
  loop ~image:initial_image ~last_action:None ~last_prediction:None ~fuel:25

(* ============================================================
   Usage
   ============================================================ *)

let main () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client = Openrouter.from_env () |> Option.get in
  let robot = () in

  match Agent.Monad.run ~sw ~env client (robot_agent ~robot "Find blue markers") with
  | Ok summary -> Printf.printf "Complete: %s\n" summary
  | Error `Max_iterations -> print_endline "Max iterations reached"
  | Error (`Api_error e) -> Printf.eprintf "API error: %s\n" (Errors.to_string e)
  | Error `No_content -> print_endline "No content in response"

(* ============================================================
   Composing Agents

   The monadic style makes it easy to compose agents:

   let research_then_summarize topic =
     let* research_result = research_agent topic in
     let* summary = summarize_agent research_result in
     return summary

   Or run agents in sequence with different models:

   let multi_model_agent query =
     let* draft =
       set_model "gpt-4" >>
       draft_agent query
     in
     let* refined =
       set_model "claude-opus" >>
       refine_agent draft
     in
     return refined

   ============================================================ *)
