# Agent Framework Design Samples

These files explore different API designs for an OCaml agent framework.
All implement the same robot control agent with:

- Vision (images from camera)
- Tool use (execute_action, task_complete)
- Multi-phase loop: Think → Act → Observe → Reflect → Repeat
- State management (messages, predictions, images)
- Termination (task_complete or max cycles)

## Versions

### v1_grammar.ml - Grammar-Style Productions
Like BNF grammar definitions. Define "production rules" that match on LLM responses.
```ocaml
grammar [
  rule @@ fun ctx ->
    match ctx.phase with
    | Observe { image; task } -> emit_user (think_prompt ~task) |> transition Thinking
    | _ -> skip
  ;
  rule @@ fun ctx ->
    match ctx.tool_call with
    | Some (Task_complete { summary }) -> terminal (Ok summary)
    | _ -> skip
]
```
**Pros:** Beautiful, declarative, composable
**Cons:** Most abstract, potentially over-engineered

### v2_pipeline_combinators.ml - Pipeline Combinators
Extend existing Pipeline with `loop`, `until`, `on_tool_call` combinators.
```ocaml
init task
|> system prompt
|> model "gpt-4"
|> loop (fun ctx ->
     think |> send_no_tools
     >>= fun _ -> request_action |> send
     >>= fun response ->
       match parse_tool response with
       | Complete s -> Stop (Ok s)
       | Execute p -> Continue (handle p))
|> max_cycles 25
```
**Pros:** Natural extension of existing API
**Cons:** Loop combinator can get complex

### v3_state_machine.ml - Explicit State Machine
Define states and transitions like a DFA.
```ocaml
machine [
  state Observing (fun s _ -> goto (Thinking s));
  state Thinking (fun s r -> emit "act now" |> goto Acting);
  state Acting (fun s r ->
    match tool r with
    | Complete -> goto (Done summary)
    | Execute -> goto Reflecting);
  terminal Done (fun s -> Ok s);
]
```
**Pros:** Explicit, easy to visualize
**Cons:** Verbose state definitions

### v4_monadic.ml - Monadic / Let Operators
Use `let*` for sequencing agent actions.
```ocaml
let* () = set_model "gpt-4" in
let rec loop ~image ~fuel =
  let* _ = send_user_with_image (think image) ~tools:[] in
  let* response = send_user "act now" ~tools in
  match parse response with
  | Complete s -> return (Ok s)
  | Execute p -> loop ~image:(capture ()) ~fuel:(fuel-1)
in
loop ~image ~fuel:25
```
**Pros:** Familiar to FP users, clean sequencing
**Cons:** Let chains can get deep

### v5_direct_recursion.ml - No DSL, Just OCaml
Plain recursive functions with pattern matching. **Works today.**
```ocaml
let rec agent ~messages ~fuel =
  if fuel = 0 then Error `Max_cycles else
  let* thinking = send thinking_prompt ~tools:[] in
  let* response = send "act now" ~tools in
  match get_tool_calls response with
  | Some [call] when matches task_complete call ->
      Ok (parse call)
  | Some [call] when matches execute_action call ->
      let result = execute (parse call) in
      agent ~messages:(add_tool_result messages result) ~fuel:(fuel-1)
  | _ -> agent ~messages ~fuel:(fuel-1)
```
**Pros:** No magic, full control, works NOW
**Cons:** Most verbose, manual everything

## Comparison

| Version | Abstraction | Learning Curve | Flexibility | Works Today |
|---------|-------------|----------------|-------------|-------------|
| Grammar | High | High | Medium | No |
| Pipeline | Medium | Low | High | No |
| State Machine | Medium | Medium | Medium | No |
| Monadic | Medium | Medium | High | No |
| Direct | None | Low | Highest | **Yes** |

## Recommendation

Start with **v5 (Direct Recursion)** as the foundation - it works today and gives
users an escape hatch. Then build **v2 (Pipeline Combinators)** on top as sugar
for common patterns.
