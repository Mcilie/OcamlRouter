(** Decision-as-tool-call agent demo *)

open Openrouter
open Openrouter.Agent

type decision =
  | Weather of string
  | Search of string

let decision_tool =
  Decision.tool ~actions:["weather"; "search"] ()

let decode_decision action args =
  let open Yojson.Safe.Util in
  match action with
  | "weather" ->
      let location = args |> member "location" |> to_string in
      Ok (Weather location)
  | "search" ->
      let query = args |> member "query" |> to_string in
      Ok (Search query)
  | other -> Error ("Unknown action: " ^ other)

type weather_params = { location : string }
type search_params = { query : string }

let weather_tool =
  Typed_tool.create
    ~name:"get_weather"
    ~description:"Get the current weather for a location"
    ~schema:Typed_tool.Schema.(obj ~required:["location"] [
      ("location", string_desc "City name");
    ])
    ~parse:(fun json ->
      let open Yojson.Safe.Util in
      { location = json |> member "location" |> to_string })
    ()

let search_tool =
  Typed_tool.create
    ~name:"web_search"
    ~description:"Search the web for information"
    ~schema:Typed_tool.Schema.(obj ~required:["query"] [
      ("query", string_desc "Search query");
    ])
    ~parse:(fun json ->
      let open Yojson.Safe.Util in
      { query = json |> member "query" |> to_string })
    ()

let get_weather location =
  Ok (Printf.sprintf "Weather in %s: 22°C and sunny" location)

let web_search query =
  Ok (Printf.sprintf "Top result for '%s': Example.com" query)

let toolset =
  Tools.(
    empty
    |> add weather_tool (fun _ctx p -> get_weather p.location)
    |> add search_tool (fun _ctx p -> web_search p.query)
  )

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in
  let ctx = Context.make ~sw ~env ~client in
  let model = "openai/gpt-5" in

  let decision_messages = [
    Message.system "Choose the action by calling the route tool with action and args.";
    Message.user "What's the weather in Paris?";
  ] in

  let decision_result =
    Chat.send ~sw ~env client
      ~model
      ~tools:[Typed_tool.to_tool decision_tool]
      ~tool_choice:Tool.Required
      ~messages:decision_messages
      ()
  in

  match decision_result with
  | Error e ->
      Printf.eprintf "Decision error: %s\n" (Errors.to_string e)
  | Ok response ->
      (match Chat.get_tool_calls response with
       | None ->
           print_endline "No decision tool call returned."
       | Some calls ->
           (match Decision.parse_tool_calls decision_tool calls with
            | Error _ ->
                print_endline "Failed to parse decision tool call."
            | Ok decision ->
                (match Decision.decode decode_decision decision with
                 | Error _ ->
                     print_endline "Failed to decode decision."
                 | Ok chosen ->
                     let prompt =
                       match chosen with
                       | Weather location ->
                           Printf.sprintf "Use get_weather to answer for %s." location
                       | Search query ->
                           Printf.sprintf "Use web_search to answer: %s." query
                     in
                     let messages = [
                       Message.system "Use tools to answer the user question.";
                       Message.user prompt;
                     ] in
                     match Auto.run ~ctx ~budget:Budget.default ~tools:toolset ~model ~messages with
                     | Error _ ->
                         print_endline "Auto tool loop failed."
                     | Ok { response; _ } ->
                         (match Chat.get_content response with
                          | Some text -> Printf.printf "Final answer: %s\n" text
                          | None -> print_endline "No final content.")))))
