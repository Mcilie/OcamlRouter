(** Version 1: Grammar-Style Productions

    Like BNF — define rules that pattern match on LLM output and produce next steps.
    Agents as grammars, responses as tokens to parse.
*)

(* NOTE: This is pseudocode/design sketch, not compilable *)

open Openrouter

(* ============================================================
   Types
   ============================================================ *)

type image = string (* base64 encoded *)

type action_params = {
  action : string;
  velocity : float option;
  duration : float option;
  angular_velocity : float option;
  expected_outcome : string;
}

type phase =
  | Observe of {
      image : image;
      last_action : string option;
      last_prediction : string option;
      task : string;
    }
  | Thinking of { image : image; task : string }
  | Acting of { task : string }

type tool_call_result =
  | Task_complete of { summary : string }
  | Execute_action of action_params
  | No_tool

(* ============================================================
   Prompts
   ============================================================ *)

let system_prompt = {|You are controlling a Unitree Go2 quadruped robot...|}

let think_prompt ~task ~image =
  Printf.sprintf "TASK: %s\n\nThis is the current view. Think about what to do." task

let reflect_prompt ~image ~action ~prediction =
  Printf.sprintf
    {|This is the view AFTER your action: %s
Your prediction was: "%s"

Did it match? What should you do next?|}
    action (Option.value prediction ~default:"none")

let format_action action params =
  Printf.sprintf "%s(velocity=%g, duration=%g)"
    action
    (Option.value params.velocity ~default:0.5)
    (Option.value params.duration ~default:1.0)

(* ============================================================
   Robot Interface (stubbed)
   ============================================================ *)

module Robot = struct
  type t = unit

  let capture_image _robot : image =
    "(base64 image data)"

  let execute _robot action params =
    Printf.printf "[EXECUTING]: %s\n" action;
    "Command sent to robot"
end

(* ============================================================
   Grammar DSL (hypothetical)
   ============================================================ *)

module Agent = struct
  module Grammar = struct
    type 'a production = {
      name : string;
      max_cycles : int option;
      handler : context -> 'a transition;
    }

    and context = {
      phase : phase;
      tool_call : tool_call_result option;
      task : string;
      messages : Message.t list;
    }

    and 'a transition =
      | Emit_user of string * 'a transition_config
      | Terminal of 'a
      | Skip

    and 'a transition_config = {
      tools : Tool.tool list;
      next_phase : phase;
    }

    let production name handler = { name; max_cycles = None; handler }

    let rule ?(max_cycles : int option) handler =
      { name = "anonymous"; max_cycles; handler }

    let emit_user msg = Emit_user (msg, { tools = []; next_phase = Acting { task = "" } })
    let no_tools t = t
    let with_tools _tools t = t
    let require_tool_call t = t
    let transition phase t = t
    let terminal value = Terminal value
    let skip = Skip

    type 'a grammar = {
      system : string;
      initial : string -> phase;
      productions : 'a production list;
    }

    let grammar ~system ~initial productions =
      { system; initial; productions }

    (* Run the grammar - tries each production until one matches *)
    let run ~sw ~env client grammar input =
      (* Implementation would:
         1. Initialize with grammar.initial input
         2. Loop: try each production's handler against current context
         3. First non-Skip result wins
         4. Apply transition, update messages, call LLM
         5. Repeat until Terminal *)
      failwith "not implemented"
  end
end

(* ============================================================
   The Robot Agent as a Grammar
   ============================================================ *)

let robot_grammar ~robot =
  let open Agent.Grammar in

  grammar
    ~system:system_prompt
    ~initial:(fun task ->
      let image = Robot.capture_image robot in
      Observe { image; last_action = None; last_prediction = None; task })

    [
      (* Rule: Observation → Thinking phase *)
      rule @@ (fun ctx ->
        match ctx.phase with
        | Observe { image; last_action; last_prediction; task } ->
            let prompt = match last_action with
              | None ->
                  think_prompt ~task ~image
              | Some action ->
                  reflect_prompt ~image ~action ~prediction:last_prediction
            in
            emit_user prompt
            |> no_tools
            |> transition (Thinking { image; task })
        | _ -> skip
      );

      (* Rule: After thinking → Request action *)
      rule @@ (fun ctx ->
        match ctx.phase with
        | Thinking { image = _; task } ->
            emit_user "Now call execute_action or task_complete."
            |> with_tools []  (* would be actual tools *)
            |> require_tool_call
            |> transition (Acting { task })
        | _ -> skip
      );

      (* Rule: task_complete called → Terminal *)
      rule @@ (fun ctx ->
        match ctx.tool_call with
        | Some (Task_complete { summary }) ->
            terminal (Ok summary)
        | _ -> skip
      );

      (* Rule: execute_action called → Execute and observe *)
      rule @@ (fun ctx ->
        match ctx.tool_call with
        | Some (Execute_action params) ->
            let result = Robot.execute robot params.action params in
            let _ = result in
            let image = Robot.capture_image robot in
            emit_user result
            |> transition (Observe {
                 image;
                 last_action = Some (format_action params.action params);
                 last_prediction = Some params.expected_outcome;
                 task = ctx.task
               })
        | _ -> skip
      );

      (* Rule: Max cycles → Terminal *)
      rule ~max_cycles:25 @@ (fun _ctx ->
        terminal (Error `Max_cycles)
      );
    ]

(* ============================================================
   Usage
   ============================================================ *)

let main () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client = Openrouter.from_env () |> Option.get in
  let robot = () in

  let result =
    robot_grammar ~robot
    |> Agent.Grammar.run ~sw ~env client "Find blue markers"
  in
  match result with
  | Ok summary -> Printf.printf "Complete: %s\n" summary
  | Error `Max_cycles -> print_endline "Max cycles reached"
