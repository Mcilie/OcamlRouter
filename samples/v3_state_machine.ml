(** Version 3: State Machine

    Define states and transitions explicitly like a DFA.
    Easy to visualize, explicit control flow.
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

(* ============================================================
   State Definition
   ============================================================ *)

type state =
  | Observing of { image : image; task : string }
  | Thinking of { image : image; task : string }
  | Acting of { task : string }
  | Reflecting of {
      image : image;
      action : string;
      prediction : string;
      task : string;
    }
  | Done of string
  | Failed of [ `Max_cycles | `Error of string ]

(* ============================================================
   Robot Interface (stubbed)
   ============================================================ *)

module Robot = struct
  type t = unit

  let capture_image _robot : image = "(base64 image data)"

  let execute _robot action _params =
    Printf.printf "[EXECUTING]: %s\n" action;
    "Command sent to robot"
end

(* ============================================================
   State Machine DSL (hypothetical)
   ============================================================ *)

module Agent = struct
  module Machine = struct
    type 'response transition_action =
      | Goto of state
      | Stay
      | Emit of {
          message : string;
          tools : Tool.tool list;
          next : state;
        }

    type 'response state_handler = 'response -> transition_action

    type transition_rule =
      | State : 'a * ('a -> Chat.response -> transition_action) -> transition_rule
      | Terminal : 'a * ('a -> ('ok, 'err) result) -> transition_rule

    type machine = {
      max_transitions : int;
      rules : transition_rule list;
    }

    let machine ~max_transitions rules =
      { max_transitions; rules }

    let state pattern handler = State (pattern, handler)
    let terminal pattern handler = Terminal (pattern, handler)

    let goto s = Goto s
    let stay = Stay
    let emit_user msg = Emit { message = msg; tools = []; next = Done "" }
    let no_tools t = t
    let with_tools _tools t = t
    let require_tool t = t

    (* Run the state machine *)
    let run ~sw:_ ~env:_ _client machine ~initial:_ =
      (* Implementation would:
         1. Start at initial state
         2. Match current state against rules
         3. Call handler with LLM response
         4. Apply transition (Goto, Stay, Emit)
         5. If Emit, add message and call LLM
         6. Repeat until terminal state or max_transitions *)
      let _ = machine in
      failwith "not implemented"
  end
end

(* ============================================================
   Prompts
   ============================================================ *)

let system_prompt = {|You are controlling a Unitree Go2 quadruped robot...|}

let think_prompt ~task ~image:_ =
  Printf.sprintf "TASK: %s\n\nThink about what to do." task

let reflect_prompt ~image:_ ~action ~prediction =
  Printf.sprintf "Action: %s, Prediction: %s. Did it match?" action prediction

let format_action action params =
  Printf.sprintf "%s(d=%g)" action (Option.value params.duration ~default:1.0)

(* ============================================================
   Response Parsing (stubbed)
   ============================================================ *)

let get_tool_call _response : [`Task_complete of string | `Execute of action_params | `None] =
  `None

let log_thinking _response = ()

(* ============================================================
   The Robot Agent as a State Machine
   ============================================================ *)

let robot_machine ~robot =
  let open Agent.Machine in

  machine ~max_transitions:50 [

    (* --------------------------------------------------------
       Observing → Thinking
       Capture image, prompt model to think
       -------------------------------------------------------- *)
    state (Observing { image = ""; task = "" }) (fun { image; task } _response ->
      emit_user (think_prompt ~task ~image)
      |> no_tools
      |> fun _ -> goto (Thinking { image; task })
    );

    (* --------------------------------------------------------
       Thinking → Acting
       After model thinks, request an action
       -------------------------------------------------------- *)
    state (Thinking { image = ""; task = "" }) (fun { task; image = _ } response ->
      log_thinking response;
      emit_user "Now call execute_action or task_complete."
      |> with_tools []  (* actual tools would go here *)
      |> require_tool
      |> fun _ -> goto (Acting { task })
    );

    (* --------------------------------------------------------
       Acting
       Handle the tool call response
       -------------------------------------------------------- *)
    state (Acting { task = "" }) (fun { task } response ->
      match get_tool_call response with
      | `Task_complete summary ->
          goto (Done summary)

      | `Execute params ->
          let _result = Robot.execute robot params.action params in
          let new_image = Robot.capture_image robot in
          goto (Reflecting {
            image = new_image;
            action = format_action params.action params;
            prediction = params.expected_outcome;
            task;
          })

      | `None ->
          (* No tool call, stay and retry *)
          stay
    );

    (* --------------------------------------------------------
       Reflecting → Thinking
       Show result image, ask for reflection, back to thinking
       -------------------------------------------------------- *)
    state (Reflecting { image = ""; action = ""; prediction = ""; task = "" })
      (fun { image; action; prediction; task } _response ->
        emit_user (reflect_prompt ~image ~action ~prediction)
        |> no_tools
        |> fun _ -> goto (Thinking { image; task })
      );

    (* --------------------------------------------------------
       Terminal States
       -------------------------------------------------------- *)
    terminal (Done "") (fun summary -> Ok summary);
    terminal (Failed `Max_cycles) (fun _err -> Error `Max_cycles);
  ]

(* ============================================================
   Usage
   ============================================================ *)

let main () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client = Openrouter.from_env () |> Option.get in
  let robot = () in

  let initial_image = Robot.capture_image robot in
  let initial = Observing { image = initial_image; task = "Find blue markers" } in

  match Agent.Machine.run ~sw ~env client (robot_machine ~robot) ~initial with
  | Ok summary -> Printf.printf "Complete: %s\n" summary
  | Error `Max_cycles -> print_endline "Max cycles reached"

(* ============================================================
   Visualization

   The state machine can be visualized as:

   ┌─────────────┐
   │  Observing  │ ──capture image──▶ ┌───────────┐
   └─────────────┘                    │ Thinking  │
         ▲                            └───────────┘
         │                                  │
         │                           "request action"
         │                                  │
         │                                  ▼
         │                            ┌───────────┐
         │        ┌───execute────────│  Acting   │
         │        │                   └───────────┘
         │        ▼                         │
   ┌─────────────┐                   task_complete
   │ Reflecting  │                          │
   └─────────────┘                          ▼
                                      ┌───────────┐
                                      │   Done    │
                                      └───────────┘
   ============================================================ *)
