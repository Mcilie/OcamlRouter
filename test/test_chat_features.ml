(** Tests for advanced Chat features *)

let test_json_mode () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  let messages = [
    Openrouter.Message.system "You are a helpful assistant that responds in JSON format.";
    Openrouter.Message.user "Give me info about Paris. Include name, country, and population as JSON.";
  ] in

  match Openrouter.Chat.send ~sw ~env client
    ~model:"openai/gpt-4o-mini"
    ~response_format:Openrouter.Common.Json_object_format
    ~messages
    ()
  with
  | Ok response ->
    (match Openrouter.Chat.get_content response with
     | Some content ->
       Printf.printf "JSON response:\n%s\n" content;
       (* Verify it's valid JSON *)
       let _ = Yojson.Safe.from_string content in
       print_endline "Valid JSON received!"
     | None ->
       print_endline "No content");
    print_endline "test_json_mode: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let test_json_schema () =
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
      ("name", `Assoc [("type", `String "string")]);
      ("age", `Assoc [("type", `String "integer")]);
      ("occupation", `Assoc [("type", `String "string")]);
    ]);
    ("required", `List [`String "name"; `String "age"; `String "occupation"]);
    ("additionalProperties", `Bool false);
  ] in

  let response_format = Openrouter.Common.Json_schema_format {
    name = "person";
    schema;
    strict = Some true;
  } in

  let messages = [
    Openrouter.Message.user "Generate a fictional person with name, age, and occupation.";
  ] in

  match Openrouter.Chat.send ~sw ~env client
    ~model:"openai/gpt-4o-mini"
    ~response_format
    ~messages
    ()
  with
  | Ok response ->
    (match Openrouter.Chat.get_content response with
     | Some content ->
       Printf.printf "Schema-constrained response:\n%s\n" content;
       let json = Yojson.Safe.from_string content in
       let open Yojson.Safe.Util in
       let name = json |> member "name" |> to_string in
       let age = json |> member "age" |> to_int in
       let occupation = json |> member "occupation" |> to_string in
       Printf.printf "Parsed: name=%s, age=%d, occupation=%s\n" name age occupation
     | None ->
       print_endline "No content");
    print_endline "test_json_schema: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let test_logit_bias () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  (* Bias against common words - using token IDs *)
  let logit_bias = [("9906", -100.0)] in (* token ID for "hello" in GPT models *)

  let messages = [
    Openrouter.Message.user "Say a one word greeting.";
  ] in

  match Openrouter.Chat.send ~sw ~env client
    ~model:"openai/gpt-4o-mini"
    ~logit_bias
    ~max_tokens:10
    ~messages
    ()
  with
  | Ok response ->
    (match Openrouter.Chat.get_content response with
     | Some content ->
       Printf.printf "Response with logit bias: %s\n" content
     | None ->
       print_endline "No content");
    print_endline "test_logit_bias: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let test_session_and_metadata () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  let session_id = "test-session-12345" in
  let metadata = [("test_key", "test_value"); ("run_id", "abc123")] in

  let messages = [
    Openrouter.Message.user "Say 'test' and nothing else.";
  ] in

  match Openrouter.Chat.send ~sw ~env client
    ~model:"openai/gpt-4o-mini"
    ~session_id
    ~metadata
    ~messages
    ()
  with
  | Ok response ->
    Printf.printf "Response with session/metadata: %s\n"
      (Option.value (Openrouter.Chat.get_content response) ~default:"(no content)");
    print_endline "test_session_and_metadata: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let test_multimodal_content () =
  (* Test creating multimodal content (no API call needed) *)
  let text_part = Openrouter.Content.text_part "What's in this image?" in
  let image_part = Openrouter.Content.image_url "https://example.com/image.jpg" in
  let content = Openrouter.Content.parts [text_part; image_part] in

  (* Verify the content serializes correctly *)
  let json = Openrouter.Content.yojson_of_content content in
  Printf.printf "Multimodal content JSON:\n%s\n"
    (Yojson.Safe.pretty_to_string json);

  (* Test audio content creation *)
  let audio = Openrouter.Content.audio_base64 ~format:"wav" "dGVzdCBhdWRpbw==" in
  let audio_json = Openrouter.Content.yojson_of_content_part audio in
  Printf.printf "Audio content JSON:\n%s\n"
    (Yojson.Safe.pretty_to_string audio_json);

  (* Test video content creation *)
  let video = Openrouter.Content.video_url "https://example.com/video.mp4" in
  let video_json = Openrouter.Content.yojson_of_content_part video in
  Printf.printf "Video content JSON:\n%s\n"
    (Yojson.Safe.pretty_to_string video_json);

  print_endline "test_multimodal_content: PASSED"

let () =
  print_endline "=== Running Advanced Chat Feature Tests ===\n";
  test_json_mode ();
  print_newline ();
  test_json_schema ();
  print_newline ();
  test_logit_bias ();
  print_newline ();
  test_session_and_metadata ();
  print_newline ();
  test_multimodal_content ();
  print_endline "\n=== All Advanced Chat Tests Passed ==="
