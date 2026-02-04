(** Demonstration of the Typed Tool System

    This example shows how to use type-safe tools with pattern matching
    instead of manually parsing JSON.

    Run with:
      eval $(opam env --switch=default) && export $(cat .env | xargs) && \
      ulimit -n 10240 && dune exec examples/typed_tools_demo.exe
*)

open Openrouter.Typed_tool

(* Define parameter types at module level *)
type weather_params = {
  location : string;
  unit : string;
}

type search_params = {
  query : string;
  num_results : int;
}

(* Define typed tools *)
let weather_tool =
  create
    ~name:"get_weather"
    ~description:"Get the current weather for a location"
    ~schema:Schema.(obj ~required:["location"; "unit"] [
      ("location", string_desc "City name, e.g. 'Paris' or 'Tokyo'");
      ("unit", enum_desc "Temperature unit" ["celsius"; "fahrenheit"]);
    ])
    ~parse:(fun json ->
      let open Yojson.Safe.Util in
      { location = json |> member "location" |> to_string;
        unit = json |> member "unit" |> to_string })
    ()

let search_tool =
  create
    ~name:"web_search"
    ~description:"Search the web for information"
    ~schema:Schema.(obj ~required:["query"; "num_results"] [
      ("query", string_desc "The search query");
      ("num_results", int_desc "Number of results to return (1-10)");
    ])
    ~parse:(fun json ->
      let open Yojson.Safe.Util in
      { query = json |> member "query" |> to_string;
        num_results = json |> member "num_results" |> to_int })
    ()

(* Fake implementations *)
let get_weather location unit =
  Printf.sprintf "Weather in %s: 22°%s, sunny"
    location (if unit = "celsius" then "C" else "F")

let web_search query num_results =
  Printf.sprintf "Found %d results for '%s': [result1, result2, ...]"
    num_results query

(* Method 1: Pattern matching with individual tools *)
let handle_with_pattern_matching tool_calls =
  print_endline "\n--- Method 1: Pattern Matching ---";
  List.iter (fun call ->
    if matches weather_tool call then begin
      let params = parse_call_exn weather_tool call in
      Printf.printf "Weather request: %s (%s)\n" params.location params.unit;
      let result = get_weather params.location params.unit in
      Printf.printf "Result: %s\n" result
    end
    else if matches search_tool call then begin
      let params = parse_call_exn search_tool call in
      Printf.printf "Search request: '%s' (max %d)\n" params.query params.num_results;
      let result = web_search params.query params.num_results in
      Printf.printf "Result: %s\n" result
    end
    else
      Printf.printf "Unknown tool: %s\n" call.function_.name
  ) tool_calls

(* Method 2: Using Toolset for automatic dispatch *)
let handle_with_toolset tool_calls =
  print_endline "\n--- Method 2: Toolset Dispatch ---";

  let toolset = Toolset.(
    empty
    |> add weather_tool (fun p -> get_weather p.location p.unit)
    |> add search_tool (fun p -> web_search p.query p.num_results)
  ) in

  List.iter (fun call ->
    match Toolset.handle toolset call with
    | Some result -> Printf.printf "Handled: %s\n" result
    | None -> Printf.printf "No handler for: %s\n" call.function_.name
  ) tool_calls

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  print_endline "=== Typed Tool System Demo ===\n";

  (* Convert typed tools to raw tools for API *)
  let tools = [to_tool weather_tool; to_tool search_tool] in

  (* Make a request that will trigger tool use *)
  print_endline "Asking: 'What's the weather like in Paris?'\n";

  let result =
    Openrouter.Chat.send ~sw ~env client
      ~model:"openai/gpt-4o-mini"
      ~tools
      ~messages:[
        Openrouter.Message.user "What's the weather like in Paris? Use celsius."
      ]
      ()
  in

  match result with
  | Ok response ->
    (match Openrouter.Chat.get_tool_calls response with
     | Some calls ->
       Printf.printf "Model requested %d tool call(s)\n" (List.length calls);

       (* Show both methods *)
       handle_with_pattern_matching calls;
       handle_with_toolset calls;

       (* Create response messages for a multi-turn conversation *)
       print_endline "\n--- Creating Tool Response Messages ---";
       let toolset = Toolset.(
         empty
         |> add weather_tool (fun p -> get_weather p.location p.unit)
         |> add search_tool (fun p -> web_search p.query p.num_results)
       ) in
       let response_msgs = Toolset.handle_all toolset calls in
       Printf.printf "Created %d response message(s)\n" (List.length response_msgs)

     | None ->
       print_endline "No tool calls (model responded directly)";
       match Openrouter.Chat.get_content response with
       | Some text -> Printf.printf "Response: %s\n" text
       | None -> ())

  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    exit 1
