(** Tests for the agent framework (non-networked) *)

open Openrouter
open Openrouter.Agent

let assert_true cond msg =
  if not cond then (Printf.eprintf "FAIL: %s\n" msg; exit 1)

let make_tool_call ~id ~name ~arguments =
  { Tool.id; type_ = "function"; function_ = { name; arguments } }

let make_response message =
  {
    Chat.id = "resp";
    model = "test";
    created = 0;
    choices = [ { Chat.index = 0; message; finish_reason = None } ];
    usage = None;
  }

let test_decision_parsing () =
  let tool = Decision.tool ~actions:["a"; "b"] () in
  let args = `Assoc [
    ("action", `String "a");
    ("args", `Assoc [("x", `Int 1)]);
  ] in
  let call = make_tool_call ~id:"1" ~name:"route"
    ~arguments:(Yojson.Safe.to_string args)
  in
  (match Decision.parse_tool_calls tool [call] with
   | Ok d -> assert_true (d.action = "a") "decision action parsed"
   | Error _ -> assert_true false "decision should parse");
  (match Decision.parse_tool_calls tool [] with
   | Error Decision.No_call -> ()
   | _ -> assert_true false "expected No_call");
  (match Decision.parse_tool_calls tool [call; call] with
   | Error Decision.Multiple_calls -> ()
   | _ -> assert_true false "expected Multiple_calls");
  let wrong = make_tool_call ~id:"2" ~name:"other"
    ~arguments:(Yojson.Safe.to_string args)
  in
  (match Decision.parse_tool_calls tool [wrong] with
   | Error (Decision.Wrong_tool _) -> ()
   | _ -> assert_true false "expected Wrong_tool")

type weather_params = { location : string }

let weather_tool =
  Typed_tool.create
    ~name:"get_weather"
    ~schema:Typed_tool.Schema.(obj ~required:["location"] [
      ("location", string);
    ])
    ~parse:(fun json ->
      let open Yojson.Safe.Util in
      { location = json |> member "location" |> to_string })
    ()

let test_toolset ctx =
  let ok_set =
    Tools.(empty |> add weather_tool (fun _ctx p -> Ok ("Weather: " ^ p.location)))
  in
  let call = make_tool_call ~id:"1" ~name:"get_weather"
    ~arguments:"{\"location\":\"Paris\"}"
  in
  (match Tools.handle_all ok_set ctx [call] with
   | Ok [msg] ->
       assert_true (msg.Message.role = Message.Tool) "tool message role";
       let content =
         match msg.Message.content with
         | Some (Content.String_content s) -> s
         | _ -> ""
       in
       assert_true (content = "Weather: Paris") "tool message content"
   | _ -> assert_true false "toolset should succeed");
  let err_set =
    Tools.(empty |> add weather_tool (fun _ctx _p -> Error { tool = "get_weather"; message = "boom" }))
  in
  (match Tools.handle_all err_set ctx [call] with
   | Error _ -> ()
   | _ -> assert_true false "expected handler error");
  let policy = { Tools.default_policy with on_unhandled = `Message } in
  let unknown = make_tool_call ~id:"2" ~name:"unknown" ~arguments:"{}" in
  (match Tools.handle_all ~policy ok_set ctx [unknown] with
   | Ok [msg] ->
       let content =
         match msg.Message.content with
         | Some (Content.String_content s) -> s
         | _ -> ""
       in
       assert_true (content = "unhandled tool: unknown") "unhandled message"
   | _ -> assert_true false "expected unhandled tool message")

let test_auto_loop ctx =
  let toolset =
    Tools.(empty |> add weather_tool (fun _ctx p -> Ok ("Weather: " ^ p.location)))
  in
  let call = make_tool_call ~id:"1" ~name:"get_weather"
    ~arguments:"{\"location\":\"Paris\"}"
  in
  let resp1 = make_response (Message.assistant_with_tool_calls [call]) in
  let resp2 = make_response (Message.assistant "done") in
  let count = ref 0 in
  let model_call ~ctx:_ ~tools:_ ~messages:_ =
    incr count;
    if !count = 1 then Ok resp1 else Ok resp2
  in
  let budget = { Budget.default with max_steps = 5; max_tool_rounds = 2; max_total_tool_calls = 5 } in
  match Auto.run ~ctx ~budget ~model_call ~tools:toolset ~model:"test"
          ~messages:[Message.user "hi"] with
  | Ok { messages; _ } ->
      assert_true (List.length messages = 4) "auto loop conversation length"
  | Error _ -> assert_true false "auto loop should succeed"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let client = Openrouter.create ~api_key:"test" () in
    let ctx = Context.make ~sw ~env ~client in
    test_decision_parsing ();
    test_toolset ctx;
    test_auto_loop ctx;
    print_endline "test_agent_decision: PASSED"
