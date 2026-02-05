(** Version 5: Direct Recursion

    No DSL magic - just plain OCaml functions, pattern matching, and recursion.
    Most verbose but works TODAY with existing library.
*)

(* NOTE: This is mostly real code that would work with current library *)

open Openrouter

(* ============================================================
   Types
   ============================================================ *)

type action_params = {
  action : string;
  velocity : float option;
  duration : float option;
  angular_velocity : float option;
  expected_outcome : string;
}

type agent_error =
  [ `Max_cycles
  | `Api_error of Errors.error
  | `Unknown_tool of string
  | `No_content
  | `Parse_error of string
  ]

(* ============================================================
   Robot Interface (stubbed for example)
   ============================================================ *)

module Robot = struct
  type t = { mutable image_counter : int }

  let create () = { image_counter = 0 }

  let capture_image robot =
    robot.image_counter <- robot.image_counter + 1;
    Printf.printf "[CAPTURE]: Image %d\n" robot.image_counter;
    (* In real code, this would capture actual image *)
    "base64_encoded_image_data_here"

  let execute _robot action params =
    Printf.printf "[EXECUTE]: %s\n" action;
    Printf.printf "  velocity: %s\n"
      (Option.fold ~none:"default" ~some:string_of_float params.velocity);
    Printf.printf "  duration: %s\n"
      (Option.fold ~none:"default" ~some:string_of_float params.duration);
    Unix.sleepf 0.5;  (* Simulate execution time *)
    "Command sent to robot"
end

(* ============================================================
   Typed Tools (using existing Typed_tool module)
   ============================================================ *)

let execute_action_tool =
  Typed_tool.create
    ~name:"execute_action"
    ~description:"Execute a robot action. Call this ONLY after thinking through the action."
    ~schema:Typed_tool.Schema.(obj ~required:["action"; "expected_outcome"] [
      ("action", enum_desc "The robot action"
         ["stand_up"; "stand_down"; "move_forward"; "move_backward";
          "turn_left"; "turn_right"; "stop"; "recovery_stand"]);
      ("velocity", number_desc "Velocity in m/s (for move_forward/backward)");
      ("duration", number_desc "Duration in seconds");
      ("angular_velocity", number_desc "Angular velocity in rad/s (for turns)");
      ("expected_outcome", string_desc "REQUIRED: Your prediction of what you expect to see");
    ])
    ~parse:(fun json ->
      let open Yojson.Safe.Util in
      {
        action = json |> member "action" |> to_string;
        velocity = json |> member "velocity" |> to_float_option;
        duration = json |> member "duration" |> to_float_option;
        angular_velocity = json |> member "angular_velocity" |> to_float_option;
        expected_outcome = json |> member "expected_outcome" |> to_string;
      })
    ()

let task_complete_tool =
  Typed_tool.create
    ~name:"task_complete"
    ~description:"Call this ONLY when you have completed the assigned task"
    ~schema:Typed_tool.Schema.(obj ~required:["summary"] [
      ("summary", string_desc "A brief summary of what was accomplished");
    ])
    ~parse:(fun json ->
      Yojson.Safe.Util.(json |> member "summary" |> to_string))
    ()

(* ============================================================
   System Prompt
   ============================================================ *)

let system_prompt = {|You are controlling a Unitree Go2 quadruped robot.

## AVAILABLE ACTIONS
- stand_up: Stand the robot up
- stand_down: Lay the robot down
- move_forward: Move forward (velocity in m/s, duration in seconds)
- move_backward: Move backward (velocity in m/s, duration in seconds)
- turn_left: Turn left (angular_velocity in rad/s, duration in seconds)
- turn_right: Turn right (angular_velocity in rad/s, duration in seconds)
- stop: Stop all movement
- recovery_stand: Emergency recovery stand

## PHYSICS CALCULATIONS
- Linear distance = velocity (m/s) × duration (s)
  Example: 0.3 m/s × 2.0 s = 0.6 meters forward
- Angular rotation = angular_velocity (rad/s) × duration (s)
  Example: 1.0 rad/s × 1.5 s = 1.5 radians ≈ 86 degrees

## IMPORTANT
- Durations below 1 second often don't work properly
- Default velocity: 0.5 m/s, default angular_velocity: 1.5 rad/s

Call task_complete when done.|}

(* ============================================================
   Helper Functions
   ============================================================ *)

let format_action params =
  match params.action with
  | "move_forward" | "move_backward" ->
      Printf.sprintf "%s(velocity=%g m/s, duration=%gs)"
        params.action
        (Option.value params.velocity ~default:0.5)
        (Option.value params.duration ~default:1.0)
  | "turn_left" | "turn_right" ->
      Printf.sprintf "%s(angular=%g rad/s, duration=%gs)"
        params.action
        (Option.value params.angular_velocity ~default:1.5)
        (Option.value params.duration ~default:1.0)
  | action -> action ^ "()"

let think_prompt ~task =
  Printf.sprintf {|TASK: %s

This is the current view from the robot's camera.

THINK about what you should do next:
- Describe what you see in the image
- Consider your options
- If you plan to move, CALCULATE: velocity × duration = distance
- Explain your reasoning

Do NOT call any tools yet. Just think and plan.|} task

let reflect_prompt ~action ~prediction =
  Printf.sprintf {|This is the view AFTER your previous action.

Your previous action was: %s
Your prediction was: "%s"

FIRST, evaluate the result:
- Did the action achieve what you predicted?
- Is the image what you expected to see?
- If not, what went wrong?

THEN, think about what you should do next.

Do NOT call any tools yet. Just think and analyze.|} action prediction

(* Convert response to assistant message for history *)
let response_to_assistant_message response =
  let content = Chat.get_content response in
  let tool_calls = Chat.get_tool_calls response in
  match content, tool_calls with
  | Some text, None -> Message.assistant text
  | _, Some calls -> Message.assistant_with_tool_calls ?content calls
  | None, None -> Message.assistant ""

(* ============================================================
   The Agent - Plain Recursive Function
   ============================================================ *)

let rec robot_agent ~sw ~env client ~robot ~messages ~last_action ~last_prediction ~fuel
  : (string, agent_error) result =

  if fuel <= 0 then Error `Max_cycles else

  (* --------------------------------------------------------
     Phase 1: Capture image
     -------------------------------------------------------- *)
  let image = Robot.capture_image robot in
  Printf.printf "\n--- CYCLE %d ---\n" (26 - fuel);

  (* --------------------------------------------------------
     Phase 2: Think (no tools)
     -------------------------------------------------------- *)
  Printf.printf "[PHASE 1: THINKING]\n";

  let think_content = match last_action with
    | None -> think_prompt ~task:(get_task messages)
    | Some action -> reflect_prompt ~action ~prediction:(Option.value last_prediction ~default:"none")
  in

  (* Create message with image *)
  let think_msg = Message.user_with_parts [
    Content.text_part think_content;
    Content.image_base64 ~media_type:"image/jpeg" image;
  ] in

  let messages = messages @ [think_msg] in

  (* Send thinking request - NO TOOLS *)
  let thinking_result =
    messages
    |> Pipeline.messages
    |> Pipeline.model "anthropic/claude-sonnet-4"
    |> Pipeline.max_tokens 1000
    |> Pipeline.run ~sw ~env client
  in

  let messages = match thinking_result with
    | Ok r ->
        (match Chat.get_content r with
         | Some text -> Printf.printf "[MODEL THINKING]:\n%s\n\n" text
         | None -> ());
        messages @ [response_to_assistant_message r]
    | Error e ->
        Printf.eprintf "API Error in thinking phase: %s\n" (Errors.to_string e);
        return (Error (`Api_error e))
  in

  (* --------------------------------------------------------
     Phase 3: Request action (with tools)
     -------------------------------------------------------- *)
  Printf.printf "[PHASE 2: ACTION REQUEST]\n";

  let action_prompt = Message.user
    "Now, based on your thinking, call either:\n\
     - execute_action: with the action, parameters, and expected_outcome\n\
     - task_complete: if you have accomplished the task\n\n\
     You MUST call one of these tools now."
  in

  let messages = messages @ [action_prompt] in

  let action_result =
    messages
    |> Pipeline.messages
    |> Pipeline.model "anthropic/claude-sonnet-4"
    |> Pipeline.tools [
         Typed_tool.to_tool execute_action_tool;
         Typed_tool.to_tool task_complete_tool;
       ]
    |> Pipeline.tool_choice Tool.Required
    |> Pipeline.run ~sw ~env client
  in

  match action_result with
  | Error e ->
      Printf.eprintf "API Error in action phase: %s\n" (Errors.to_string e);
      Error (`Api_error e)

  | Ok response ->
      let messages = messages @ [response_to_assistant_message response] in

      (* --------------------------------------------------------
         Phase 4: Handle tool calls
         -------------------------------------------------------- *)
      match Chat.get_tool_calls response with
      | None ->
          Printf.printf "No tool call made - retrying...\n";
          robot_agent ~sw ~env client ~robot ~messages
            ~last_action ~last_prediction ~fuel:(fuel - 1)

      | Some [] ->
          Printf.printf "Empty tool calls - retrying...\n";
          robot_agent ~sw ~env client ~robot ~messages
            ~last_action ~last_prediction ~fuel:(fuel - 1)

      | Some (call :: _) ->
          (* Check for task_complete *)
          if Typed_tool.matches task_complete_tool call then begin
            let summary = Typed_tool.parse_call_exn task_complete_tool call in
            Printf.printf "\n[TASK COMPLETE]: %s\n" summary;
            let tool_msg = Message.tool ~tool_call_id:call.id
              (Printf.sprintf "TASK_COMPLETE: %s" summary) in
            let _messages = messages @ [tool_msg] in
            Ok summary
          end

          (* Check for execute_action *)
          else if Typed_tool.matches execute_action_tool call then begin
            let params = Typed_tool.parse_call_exn execute_action_tool call in

            Printf.printf "[EXECUTING]: %s\n" (format_action params);
            Printf.printf "[PREDICTION]: %s\n" params.expected_outcome;

            (* Execute the action *)
            let result = Robot.execute robot params.action params in
            Printf.printf "[RESULT]: %s\n" result;

            (* Add tool result to messages *)
            let tool_msg = Message.tool ~tool_call_id:call.id result in
            let messages = messages @ [tool_msg] in

            (* Recurse with updated state *)
            robot_agent ~sw ~env client ~robot ~messages
              ~last_action:(Some (format_action params))
              ~last_prediction:(Some params.expected_outcome)
              ~fuel:(fuel - 1)
          end

          else begin
            Printf.eprintf "Unknown tool: %s\n" call.function_.name;
            Error (`Unknown_tool call.function_.name)
          end

(* Helper to extract task from initial messages *)
and get_task messages =
  List.find_map (fun (msg : Message.t) ->
    if msg.role = Message.User then
      match msg.content with
      | Some (Content.String_content s) -> Some s
      | _ -> None
    else None
  ) messages
  |> Option.value ~default:"Unknown task"

and return x = x

(* ============================================================
   Entry Point
   ============================================================ *)

let run_robot_agent ~sw ~env client ~robot task =
  let initial_messages = [
    Message.system system_prompt;
    Message.user task;
  ] in

  robot_agent ~sw ~env client ~robot
    ~messages:initial_messages
    ~last_action:None
    ~last_prediction:None
    ~fuel:25

(* ============================================================
   Usage
   ============================================================ *)

let main () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Set OPENROUTER_API_KEY"
  in

  let robot = Robot.create () in

  print_endline "========================================";
  print_endline "Starting Robot Agent (Direct Recursion)";
  print_endline "========================================";

  match run_robot_agent ~sw ~env client ~robot "Find blue markers on the floor" with
  | Ok summary ->
      print_endline "\n========================================";
      Printf.printf "Task completed: %s\n" summary;
      print_endline "========================================"
  | Error `Max_cycles ->
      print_endline "\nMax cycles reached"
  | Error (`Api_error e) ->
      Printf.eprintf "\nAPI error: %s\n" (Errors.to_string e)
  | Error (`Unknown_tool name) ->
      Printf.eprintf "\nUnknown tool: %s\n" name
  | Error `No_content ->
      print_endline "\nNo content in response"
  | Error (`Parse_error msg) ->
      Printf.eprintf "\nParse error: %s\n" msg

(* ============================================================
   Why This Approach Works

   Pros:
   - No magic, just OCaml
   - Full control over every step
   - Easy to debug and understand
   - Works TODAY with existing library
   - Pattern matching is natural

   Cons:
   - Verbose
   - Manual message management
   - Easy to make mistakes with state

   This is the "escape hatch" - if the DSL doesn't fit your
   use case, you can always drop down to direct recursion.

   ============================================================ *)
