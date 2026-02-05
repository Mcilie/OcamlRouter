(** Version 2: Pipeline Combinators

    Extend Pipeline with loop, until, branch combinators.
    Natural extension of existing Pipeline API.
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
  | Task_complete of { summary : string }
  | Execute_action of action_params
  | No_tool

(* ============================================================
   Robot Interface (stubbed)
   ============================================================ *)

module Robot = struct
  type t = unit

  let capture_image _robot : image = "(base64 image data)"

  let execute _robot _action _params =
    "Command sent to robot"
end

(* ============================================================
   Extended Pipeline DSL (hypothetical)
   ============================================================ *)

module Agent = struct
  module Pipeline = struct
    (* Loop state passed to each iteration *)
    type 'state loop_ctx = {
      current_image : image;
      last_action : string option;
      last_prediction : string option;
      state : 'state;
      iteration : int;
    }

    (* Loop control flow *)
    type ('state, 'result) loop_result =
      | Continue of 'state loop_ctx
      | Stop of 'result

    (* Agent pipeline builder *)
    type ('state, 'result) t = {
      system : string option;
      model : string option;
      tools : Tool.tool list;
      max_cycles : int;
      initial_state : 'state;
    }

    let init _task = {
      system = None;
      model = None;
      tools = [];
      max_cycles = 25;
      initial_state = ();
    }

    let system prompt t = { t with system = Some prompt }
    let model m t = { t with model = Some m }
    let max_cycles n t = { t with max_cycles = n }

    (* Observe: capture image at start of each loop *)
    let observe capture_fn t =
      (* Store the capture function for use in loop *)
      t

    (* The main loop combinator *)
    let loop handler t =
      (* This would:
         1. Call LLM with current messages
         2. Pass response to handler
         3. Handler returns Continue or Stop
         4. If Continue, update state and repeat
         5. If Stop, return result *)
      t

    (* Alternative: declarative stop condition *)
    let until condition t = t
    let on_tool_call handler t = t
    let on_max_cycles handler t = t

    (* Monadic bind for sequencing within loop *)
    let ( >>= ) _ma _f = failwith "not implemented"

    let send_no_tools _prompt = failwith "not implemented"
    let send _prompt = failwith "not implemented"
    let tool_result _result = failwith "not implemented"
    let with_state _new_state _x = failwith "not implemented"

    let run ~sw:_ ~env:_ _client _pipeline =
      failwith "not implemented"
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
- Linear distance = velocity (m/s) × duration (s)
- Angular rotation = angular_velocity (rad/s) × duration (s)

Call task_complete when done.|}

let think_prompt ~task ~image:_ =
  Printf.sprintf "TASK: %s\n\nThis is the current view. Think about what to do." task

let reflect_prompt ~action ~prediction =
  Printf.sprintf
    {|This is the view AFTER your action: %s
Your prediction was: "%s"

Did it match? What should you do next?|}
    action prediction

let format_action params =
  Printf.sprintf "%s(v=%g, d=%g)"
    params.action
    (Option.value params.velocity ~default:0.5)
    (Option.value params.duration ~default:1.0)

let parse_tool_call _response : tool_call_result =
  (* Would parse actual tool calls from response *)
  No_tool

(* ============================================================
   The Robot Agent with Pipeline Combinators
   ============================================================ *)

let robot_agent ~robot task =
  let open Agent.Pipeline in

  (* Define the phases as pipeline stages *)
  let think ~image ~last_action ~last_prediction =
    match last_action with
    | None -> think_prompt ~task ~image
    | Some action -> reflect_prompt ~action ~prediction:(Option.value last_prediction ~default:"")
  in

  init task
  |> system system_prompt
  |> model "anthropic/claude-sonnet-4"

  (* Initial observation *)
  |> observe (fun _ctx -> Robot.capture_image robot)

  (* Main loop *)
  |> loop (fun ctx ->
       (* Phase 1: Think (no tools) *)
       let image = ctx.current_image in
       think ~image ~last_action:ctx.last_action ~last_prediction:ctx.last_prediction
       |> send_no_tools

       (* Phase 2: Request action *)
       >>= fun _thinking ->
       "Now call execute_action or task_complete."
       |> send

       (* Phase 3: Handle response *)
       >>= fun response ->
       match parse_tool_call response with
       | Task_complete { summary } ->
           Stop (Ok summary)

       | Execute_action params ->
           let result = Robot.execute robot params.action params in
           let _ = result in
           let new_image = Robot.capture_image robot in
           tool_result result
           |> with_state {
                ctx with
                last_action = Some (format_action params);
                last_prediction = Some params.expected_outcome;
                current_image = new_image;
              }
           |> fun _ -> Continue ctx  (* simplified *)

       | No_tool ->
           Continue ctx  (* retry *)
     )

  |> max_cycles 25
  |> on_max_cycles (fun _ctx -> Error `Max_cycles)

(* ============================================================
   Alternative: More Declarative Style
   ============================================================ *)

let robot_agent_declarative ~robot:_ _task =
  let open Agent.Pipeline in

  init "task"
  |> system system_prompt
  |> model "anthropic/claude-sonnet-4"

  (* Declarative style with combinators *)
  |> until (fun response ->
       (* Stop when task_complete is called *)
       match parse_tool_call response with
       | Task_complete _ -> true
       | _ -> false)

  |> on_tool_call (fun call ->
       (* Auto-handle tool calls *)
       match call with
       | Execute_action params ->
           let result = "executed" in
           Some result
       | _ -> None)

  |> max_cycles 25

(* ============================================================
   Usage
   ============================================================ *)

let main () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client = Openrouter.from_env () |> Option.get in
  let robot = () in

  match robot_agent ~robot "Find blue markers" |> Agent.Pipeline.run ~sw ~env client with
  | Ok summary -> Printf.printf "Complete: %s\n" summary
  | Error `Max_cycles -> print_endline "Max cycles reached"
