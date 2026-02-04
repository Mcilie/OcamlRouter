(** Tool use example *)

let weather_tool = Openrouter.Tool.make_function
  ~description:"Get the current weather for a location"
  ~parameters:(`Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("location", `Assoc [
        ("type", `String "string");
        ("description", `String "The city and state, e.g. San Francisco, CA")
      ])
    ]);
    ("required", `List [`String "location"])
  ])
  "get_weather"

let get_weather location =
  (* Fake implementation *)
  Printf.sprintf "The weather in %s is sunny and 72F" location

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  (* Create client from environment variable *)
  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY environment variable"
  in

  (* Initial messages *)
  let messages = [
    Openrouter.Message.user "What's the weather like in Paris?";
  ] in

  (* Send request with tools *)
  match Openrouter.Chat.send ~sw ~env client
    ~model:"openai/gpt-5"
    ~tools:[weather_tool]
    ~messages
    ()
  with
  | Ok response ->
    (match Openrouter.Chat.get_tool_calls response with
     | Some tool_calls ->
       Printf.printf "Model wants to call %d tool(s)\n" (List.length tool_calls);
       List.iter (fun (tc : Openrouter.Tool.tool_call) ->
         Printf.printf "Tool: %s\n" tc.function_.name;
         Printf.printf "Arguments: %s\n" tc.function_.arguments;

         (* Parse arguments and call the function *)
         let args = Yojson.Safe.from_string tc.function_.arguments in
         let location = Yojson.Safe.Util.(args |> member "location" |> to_string) in
         let result = get_weather location in
         Printf.printf "Result: %s\n" result
       ) tool_calls
     | None ->
       match Openrouter.Chat.get_content response with
       | Some content -> print_endline content
       | None -> print_endline "No response")
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e)
