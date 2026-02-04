(** Tests for the composable Pipeline builder *)

open Openrouter.Pipeline

let test_basic_pipeline () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  (* Simple pipeline: prompt |> model |> run *)
  let result =
    "What is 2 + 2? Reply with just the number."
    |> prompt
    |> model "openai/gpt-4o-mini"
    |> max_tokens 10
    |> run ~sw ~env client
  in

  match result with
  | Ok response ->
    let content = Openrouter.Chat.get_content response in
    Printf.printf "Basic pipeline result: %s\n"
      (Option.value content ~default:"(no content)");
    print_endline "test_basic_pipeline: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let test_pipeline_with_system () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  (* Pipeline with system message *)
  let result =
    "Hello!"
    |> prompt
    |> system "You are a pirate. Always respond like a pirate."
    |> model "openai/gpt-4o-mini"
    |> max_tokens 50
    |> temperature 0.7
    |> run ~sw ~env client
  in

  match result with
  | Ok response ->
    let content = Openrouter.Chat.get_content response in
    Printf.printf "Pipeline with system: %s\n"
      (Option.value content ~default:"(no content)");
    print_endline "test_pipeline_with_system: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let test_pipeline_json_mode () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  (* Pipeline with JSON mode *)
  let result =
    "Give me a JSON object with fields: name, age. Make up values."
    |> prompt
    |> system "Always respond with valid JSON only."
    |> model "openai/gpt-4o-mini"
    |> json_mode
    |> max_tokens 100
    |> run ~sw ~env client
  in

  match result with
  | Ok response ->
    (match Openrouter.Chat.get_content response with
     | Some content ->
       Printf.printf "Pipeline JSON mode: %s\n" content;
       let _ = Yojson.Safe.from_string content in
       print_endline "Valid JSON received!"
     | None ->
       print_endline "No content");
    print_endline "test_pipeline_json_mode: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let test_pipeline_json_schema () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  let schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("city", `Assoc [("type", `String "string")]);
      ("country", `Assoc [("type", `String "string")]);
    ]);
    ("required", `List [`String "city"; `String "country"]);
    ("additionalProperties", `Bool false);
  ] in

  (* Pipeline with JSON schema *)
  let result =
    "Name a random capital city."
    |> prompt
    |> model "openai/gpt-4o-mini"
    |> json_schema ~name:"capital" ~strict:true schema
    |> max_tokens 100
    |> run ~sw ~env client
  in

  match result with
  | Ok response ->
    (match Openrouter.Chat.get_content response with
     | Some content ->
       Printf.printf "Pipeline JSON schema: %s\n" content;
       let json = Yojson.Safe.from_string content in
       let open Yojson.Safe.Util in
       let city = json |> member "city" |> to_string in
       let country = json |> member "country" |> to_string in
       Printf.printf "Parsed: city=%s, country=%s\n" city country
     | None ->
       print_endline "No content");
    print_endline "test_pipeline_json_schema: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let test_pipeline_streaming () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  (* Pipeline with streaming *)
  let result =
    "Count from 1 to 5, one number per line."
    |> prompt
    |> model "openai/gpt-4o-mini"
    |> max_tokens 50
    |> run_stream ~sw ~env client
  in

  match result with
  | Ok stream ->
    print_string "Pipeline streaming: ";
    flush stdout;
    let content = Openrouter.Chat.collect_content stream in
    print_endline content;
    print_endline "test_pipeline_streaming: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let test_pipeline_conversation () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  (* Build a conversation using messages function *)
  let conv = [
    Openrouter.Message.system "You are a math tutor.";
    Openrouter.Message.user "What is 5 * 5?";
    Openrouter.Message.assistant "5 * 5 = 25";
    Openrouter.Message.user "What about 6 * 6?";
  ] in

  let result =
    conv
    |> messages
    |> model "openai/gpt-4o-mini"
    |> max_tokens 20
    |> run ~sw ~env client
  in

  match result with
  | Ok response ->
    let content = Openrouter.Chat.get_content response in
    Printf.printf "Pipeline conversation: %s\n"
      (Option.value content ~default:"(no content)");
    print_endline "test_pipeline_conversation: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let test_pipeline_chained () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  (* Heavily chained pipeline demonstrating all the options *)
  let result =
    "What color is the sky?"
    |> prompt
    |> system "You give very brief answers, just a few words."
    |> model "openai/gpt-4o-mini"
    |> temperature 0.5
    |> max_tokens 10
    |> session_id "test-session-pipeline"
    |> metadata [("test", "pipeline")]
    |> run ~sw ~env client
  in

  match result with
  | Ok response ->
    let content = Openrouter.Chat.get_content response in
    Printf.printf "Pipeline chained: %s\n"
      (Option.value content ~default:"(no content)");
    print_endline "test_pipeline_chained: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let () =
  print_endline "=== Running Pipeline Tests ===\n";
  test_basic_pipeline ();
  print_newline ();
  test_pipeline_with_system ();
  print_newline ();
  test_pipeline_json_mode ();
  print_newline ();
  test_pipeline_json_schema ();
  print_newline ();
  test_pipeline_streaming ();
  print_newline ();
  test_pipeline_conversation ();
  print_newline ();
  test_pipeline_chained ();
  print_endline "\n=== All Pipeline Tests Passed ==="
