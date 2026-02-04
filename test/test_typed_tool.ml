(** Tests for the Typed Tool System *)

open Openrouter.Typed_tool

(* Type definitions for tests - must be at module level *)
type weather_params = {
  location : string;
  unit : string option;
}

type search_params = {
  query : string;
  limit : int option;
}

type calc_params = {
  operation : string;
  a : int;
  b : int;
}

(* Test schema builders *)
let test_schema_builders () =
  print_endline "Testing schema builders...";

  (* Simple types *)
  let str_schema = Schema.string in
  assert (str_schema = `Assoc [("type", `String "string")]);

  let int_schema = Schema.int in
  assert (int_schema = `Assoc [("type", `String "integer")]);

  let num_schema = Schema.number in
  assert (num_schema = `Assoc [("type", `String "number")]);

  let bool_schema = Schema.boolean in
  assert (bool_schema = `Assoc [("type", `String "boolean")]);

  (* Enum *)
  let enum_schema = Schema.enum ["celsius"; "fahrenheit"] in
  let expected = `Assoc [
    ("type", `String "string");
    ("enum", `List [`String "celsius"; `String "fahrenheit"]);
  ] in
  assert (enum_schema = expected);

  (* Array *)
  let arr_schema = Schema.array Schema.string in
  let expected = `Assoc [
    ("type", `String "array");
    ("items", `Assoc [("type", `String "string")]);
  ] in
  assert (arr_schema = expected);

  (* Object *)
  let obj_schema = Schema.obj ~required:["name"] [
    ("name", Schema.string);
    ("age", Schema.int);
  ] in
  (match obj_schema with
   | `Assoc fields ->
     assert (List.assoc "type" fields = `String "object");
     assert (List.mem_assoc "properties" fields);
     assert (List.assoc "required" fields = `List [`String "name"]);
   | _ -> assert false);

  print_endline "test_schema_builders: PASSED"

(* Test typed tool creation and conversion *)
let test_typed_tool_creation () =
  print_endline "Testing typed tool creation...";

  let weather_tool = create
    ~name:"get_weather"
    ~description:"Get current weather for a location"
    ~schema:Schema.(obj ~required:["location"] [
      ("location", string_desc "The city name");
      ("unit", enum ["celsius"; "fahrenheit"]);
    ])
    ~parse:(fun json ->
      let open Yojson.Safe.Util in
      { location = json |> member "location" |> to_string;
        unit = json |> member "unit" |> to_string_option })
    ()
  in

  (* Check the typed tool *)
  assert (weather_tool.name = "get_weather");
  assert (weather_tool.description = Some "Get current weather for a location");
  assert (weather_tool.strict = true);

  (* Convert to raw tool *)
  let raw_tool = to_tool weather_tool in
  assert (raw_tool.function_.name = "get_weather");
  assert (raw_tool.function_.description = Some "Get current weather for a location");

  print_endline "test_typed_tool_creation: PASSED"

(* Test parsing tool calls *)
let test_parse_tool_call () =
  print_endline "Testing tool call parsing...";

  let search_tool = create
    ~name:"search"
    ~description:"Search for information"
    ~schema:Schema.(obj ~required:["query"] [
      ("query", string);
      ("limit", int);
    ])
    ~parse:(fun json ->
      let open Yojson.Safe.Util in
      { query = json |> member "query" |> to_string;
        limit = json |> member "limit" |> to_int_option })
    ()
  in

  (* Create a mock tool call *)
  let tool_call : Openrouter.Tool.tool_call = {
    id = "call_123";
    type_ = "function";
    function_ = {
      name = "search";
      arguments = {|{"query": "OCaml tutorials", "limit": 10}|};
    };
  } in

  (* Test matches *)
  assert (matches search_tool tool_call = true);

  (* Test parse_call *)
  (match parse_call search_tool tool_call with
   | Some params ->
     assert (params.query = "OCaml tutorials");
     assert (params.limit = Some 10);
   | None -> assert false);

  (* Test parse_call_exn *)
  let params = parse_call_exn search_tool tool_call in
  assert (params.query = "OCaml tutorials");

  (* Test non-matching tool call *)
  let other_call : Openrouter.Tool.tool_call = {
    id = "call_456";
    type_ = "function";
    function_ = { name = "other_tool"; arguments = "{}"; };
  } in
  assert (matches search_tool other_call = false);
  assert (parse_call search_tool other_call = None);

  print_endline "test_parse_tool_call: PASSED"

(* Test with_id parsing *)
let test_parse_with_id () =
  print_endline "Testing parse with ID...";

  let echo_tool = create
    ~name:"echo"
    ~schema:Schema.(obj ~required:["message"] [("message", string)])
    ~parse:(fun json ->
      Yojson.Safe.Util.(json |> member "message" |> to_string))
    ()
  in

  let tool_call : Openrouter.Tool.tool_call = {
    id = "call_789";
    type_ = "function";
    function_ = { name = "echo"; arguments = {|{"message": "hello"}|}; };
  } in

  match parse_call_with_id echo_tool tool_call with
  | Some parsed ->
    assert (parsed.id = "call_789");
    assert (parsed.params = "hello");
    print_endline "test_parse_with_id: PASSED"
  | None -> assert false

(* Test Toolset *)
let test_toolset () =
  print_endline "Testing Toolset...";

  (* Define two tools *)
  let add_tool = create
    ~name:"add"
    ~schema:Schema.(obj ~required:["a"; "b"] [("a", int); ("b", int)])
    ~parse:(fun json ->
      let open Yojson.Safe.Util in
      (json |> member "a" |> to_int,
       json |> member "b" |> to_int))
    ()
  in

  let multiply_tool = create
    ~name:"multiply"
    ~schema:Schema.(obj ~required:["a"; "b"] [("a", int); ("b", int)])
    ~parse:(fun json ->
      let open Yojson.Safe.Util in
      (json |> member "a" |> to_int,
       json |> member "b" |> to_int))
    ()
  in

  (* Create toolset *)
  let toolset = Toolset.(
    empty
    |> add add_tool (fun (a, b) -> string_of_int (a + b))
    |> add multiply_tool (fun (a, b) -> string_of_int (a * b))
  ) in

  (* Check tools are registered *)
  let tools = Toolset.to_tools toolset in
  assert (List.length tools = 2);

  (* Test handling add *)
  let add_call : Openrouter.Tool.tool_call = {
    id = "call_add";
    type_ = "function";
    function_ = { name = "add"; arguments = {|{"a": 3, "b": 5}|}; };
  } in
  (match Toolset.handle toolset add_call with
   | Some result -> assert (result = "8")
   | None -> assert false);

  (* Test handling multiply *)
  let mult_call : Openrouter.Tool.tool_call = {
    id = "call_mult";
    type_ = "function";
    function_ = { name = "multiply"; arguments = {|{"a": 4, "b": 7}|}; };
  } in
  (match Toolset.handle toolset mult_call with
   | Some result -> assert (result = "28")
   | None -> assert false);

  (* Test handle_to_message *)
  (match Toolset.handle_to_message toolset add_call with
   | Some msg ->
     assert (msg.role = Openrouter.Message.Tool);
     assert (msg.tool_call_id = Some "call_add");
   | None -> assert false);

  (* Test unknown tool *)
  let unknown_call : Openrouter.Tool.tool_call = {
    id = "call_unknown";
    type_ = "function";
    function_ = { name = "unknown"; arguments = "{}"; };
  } in
  assert (Toolset.handle toolset unknown_call = None);

  print_endline "test_toolset: PASSED"

(* Integration test with real API *)
let test_real_api () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  print_endline "Testing typed tools with real API...";

  let calc_tool = create
    ~name:"calculate"
    ~description:"Perform basic arithmetic"
    ~schema:Schema.(obj ~required:["operation"; "a"; "b"] [
      ("operation", enum_desc "The operation to perform" ["add"; "subtract"; "multiply"; "divide"]);
      ("a", int_desc "First operand");
      ("b", int_desc "Second operand");
    ])
    ~parse:(fun json ->
      let open Yojson.Safe.Util in
      { operation = json |> member "operation" |> to_string;
        a = json |> member "a" |> to_int;
        b = json |> member "b" |> to_int })
    ()
  in

  (* Make API call with typed tool *)
  let result =
    Openrouter.Chat.send ~sw ~env client
      ~model:"openai/gpt-4o-mini"
      ~tools:[to_tool calc_tool]
      ~messages:[Openrouter.Message.user "What is 42 multiplied by 17?"]
      ()
  in

  match result with
  | Ok response ->
    (match Openrouter.Chat.get_tool_calls response with
     | Some [call] ->
       (* Parse with typed tool *)
       (match parse_call calc_tool call with
        | Some params ->
          Printf.printf "Got tool call: %s(%d, %d)\n"
            params.operation params.a params.b;
          (* Verify it makes sense *)
          assert (params.operation = "multiply");
          assert ((params.a = 42 && params.b = 17) || (params.a = 17 && params.b = 42));
          print_endline "test_real_api: PASSED"
        | None ->
          Printf.eprintf "Failed to parse tool call\n";
          assert false)
     | Some calls ->
       Printf.eprintf "Expected 1 tool call, got %d\n" (List.length calls);
       assert false
     | None ->
       Printf.eprintf "No tool calls in response\n";
       assert false)
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let () =
  print_endline "=== Running Typed Tool Tests ===\n";
  test_schema_builders ();
  print_newline ();
  test_typed_tool_creation ();
  print_newline ();
  test_parse_tool_call ();
  print_newline ();
  test_parse_with_id ();
  print_newline ();
  test_toolset ();
  print_newline ();
  test_real_api ();
  print_endline "\n=== All Typed Tool Tests Passed ==="
