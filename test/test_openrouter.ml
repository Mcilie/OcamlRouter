(** Tests for OpenRouter SDK *)

(* Test version *)
let test_version () =
  Alcotest.(check string) "version" "0.1.0" Openrouter.version

(* Test message creation *)
let test_message_system () =
  let msg = Openrouter.Message.system "You are helpful." in
  Alcotest.(check bool) "role is system"
    (msg.role = Openrouter.Message.System) true

let test_message_user () =
  let msg = Openrouter.Message.user "Hello" in
  Alcotest.(check bool) "role is user"
    (msg.role = Openrouter.Message.User) true

let test_message_assistant () =
  let msg = Openrouter.Message.assistant "Hi there!" in
  Alcotest.(check bool) "role is assistant"
    (msg.role = Openrouter.Message.Assistant) true

let test_message_tool () =
  let msg = Openrouter.Message.tool ~tool_call_id:"tc_123" "result" in
  Alcotest.(check bool) "role is tool"
    (msg.role = Openrouter.Message.Tool) true;
  Alcotest.(check (option string)) "has tool_call_id"
    (Some "tc_123") msg.tool_call_id

(* Test tool creation *)
let test_tool_creation () =
  let tool = Openrouter.Tool.make_function
    ~description:"Test function"
    ~parameters:(`Assoc [("type", `String "object")])
    "test_func"
  in
  Alcotest.(check string) "type is function" "function" tool.type_;
  Alcotest.(check string) "name is correct" "test_func" tool.function_.name;
  Alcotest.(check (option string)) "description is set"
    (Some "Test function") tool.function_.description

(* Test JSON serialization *)
let test_usage_json () =
  let usage : Openrouter.Common.usage = {
    prompt_tokens = 10;
    completion_tokens = 20;
    total_tokens = 30;
  } in
  let json = Openrouter.Common.yojson_of_usage usage in
  let usage' = Openrouter.Common.usage_of_yojson json in
  Alcotest.(check int) "prompt_tokens" 10 usage'.prompt_tokens;
  Alcotest.(check int) "completion_tokens" 20 usage'.completion_tokens;
  Alcotest.(check int) "total_tokens" 30 usage'.total_tokens

(* Test reasoning configuration *)
let test_reasoning () =
  let r = Openrouter.Reasoning.make ~effort:Openrouter.Reasoning.High () in
  Alcotest.(check bool) "effort is high"
    (r.effort = Some Openrouter.Reasoning.High) true

(* Test provider preferences *)
let test_provider_preferences () =
  let prefs = Openrouter.Provider.make_preferences
    ~allow_fallbacks:true
    ~order:["openai"; "anthropic"]
    ()
  in
  Alcotest.(check (option bool)) "allow_fallbacks"
    (Some true) prefs.allow_fallbacks;
  Alcotest.(check (option (list string))) "order"
    (Some ["openai"; "anthropic"]) prefs.order

(* Test SSE parsing *)
let test_sse_parse_single_event () =
  let input = "event: message\ndata: {\"test\": 1}\n\n" in
  let events = Openrouter.Sse.parse_chunk input in
  Alcotest.(check int) "one event" 1 (List.length events);
  let event = List.hd events in
  Alcotest.(check (option string)) "event type"
    (Some "message") event.event_type;
  Alcotest.(check string) "data" "{\"test\": 1}" event.data

let test_sse_parse_multiple_events () =
  let input = "data: first\n\ndata: second\n\n" in
  let events = Openrouter.Sse.parse_chunk input in
  Alcotest.(check int) "two events" 2 (List.length events);
  Alcotest.(check string) "first data" "first" (List.hd events).data;
  Alcotest.(check string) "second data" "second" (List.nth events 1).data

let test_sse_done_event () =
  let state = Openrouter.Sse.Stream.create () in
  Openrouter.Sse.Stream.feed state "data: [DONE]\n\n";
  let event = Openrouter.Sse.Stream.next_event state in
  Alcotest.(check bool) "no event returned (DONE)" true (Option.is_none event);
  Alcotest.(check bool) "stream is done" true (Openrouter.Sse.Stream.is_done state)

(* Test config *)
let test_config_default () =
  let config = Openrouter.Config.default in
  Alcotest.(check string) "base_url"
    "https://openrouter.ai/api/v1" config.base_url

let test_config_custom () =
  let config = Openrouter.Config.create
    ~base_url:"https://custom.api.com"
    ~timeout:30.0
    ~app_name:"TestApp"
    ()
  in
  Alcotest.(check string) "custom base_url"
    "https://custom.api.com" config.base_url;
  Alcotest.(check (float 0.1)) "custom timeout" 30.0 config.timeout;
  Alcotest.(check (option string)) "app_name"
    (Some "TestApp") config.app_name

(* Test client creation *)
let test_client_creation () =
  let client = Openrouter.create ~api_key:"sk-test" () in
  Alcotest.(check string) "api_key"
    "sk-test" (Openrouter.Client.api_key client);
  Alcotest.(check string) "base_url"
    "https://openrouter.ai/api/v1" (Openrouter.Client.base_url client)

(* Test auth *)
let test_auth_header () =
  let auth = Openrouter.Auth.create "sk-test-key" in
  let (name, value) = Openrouter.Auth.authorization_header auth in
  Alcotest.(check string) "header name" "Authorization" name;
  Alcotest.(check string) "header value" "Bearer sk-test-key" value

let () =
  Alcotest.run "Openrouter" [
    "basic", [
      Alcotest.test_case "version" `Quick test_version;
    ];
    "message", [
      Alcotest.test_case "system message" `Quick test_message_system;
      Alcotest.test_case "user message" `Quick test_message_user;
      Alcotest.test_case "assistant message" `Quick test_message_assistant;
      Alcotest.test_case "tool message" `Quick test_message_tool;
    ];
    "tool", [
      Alcotest.test_case "tool creation" `Quick test_tool_creation;
    ];
    "json", [
      Alcotest.test_case "usage roundtrip" `Quick test_usage_json;
    ];
    "reasoning", [
      Alcotest.test_case "reasoning config" `Quick test_reasoning;
    ];
    "provider", [
      Alcotest.test_case "provider preferences" `Quick test_provider_preferences;
    ];
    "sse", [
      Alcotest.test_case "parse single event" `Quick test_sse_parse_single_event;
      Alcotest.test_case "parse multiple events" `Quick test_sse_parse_multiple_events;
      Alcotest.test_case "done event" `Quick test_sse_done_event;
    ];
    "config", [
      Alcotest.test_case "default config" `Quick test_config_default;
      Alcotest.test_case "custom config" `Quick test_config_custom;
    ];
    "client", [
      Alcotest.test_case "client creation" `Quick test_client_creation;
    ];
    "auth", [
      Alcotest.test_case "authorization header" `Quick test_auth_header;
    ];
  ]
