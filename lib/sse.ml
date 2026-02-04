(** Server-Sent Events parser *)

type event = {
  event_type : string option;
  data : string;
  id : string option;
  retry : int option;
}

let empty_event = { event_type = None; data = ""; id = None; retry = None }

type line =
  | Field of string * string
  | Empty
  | Comment

let parse_line line =
  if String.length line = 0 then Empty
  else if line.[0] = ':' then Comment
  else
    match String.index_opt line ':' with
    | None -> Field (line, "")
    | Some i ->
        let field = String.sub line 0 i in
        let value_start = if i + 1 < String.length line && line.[i + 1] = ' ' then i + 2 else i + 1 in
        let value = String.sub line value_start (String.length line - value_start) in
        Field (field, value)

let apply_field event field value =
  match field with
  | "event" -> { event with event_type = Some value }
  | "data" ->
      let data =
        if String.length event.data = 0 then value else event.data ^ "\n" ^ value
      in
      { event with data }
  | "id" -> { event with id = Some value }
  | "retry" -> (
      match int_of_string_opt value with
      | Some n -> { event with retry = Some n }
      | None -> event)
  | _ -> event

let is_done_event event =
  String.trim event.data = "[DONE]"

module Stream = struct
  type state = {
    mutable buffer : string;
    mutable current_event : event;
    mutable done_ : bool;
  }

  let create () = { buffer = ""; current_event = empty_event; done_ = false }

  let feed state chunk =
    state.buffer <- state.buffer ^ chunk

  let next_event state =
    if state.done_ then None
    else
      let rec process_lines () =
        match String.index_opt state.buffer '\n' with
        | None -> None
        | Some i ->
            let line =
              let raw = String.sub state.buffer 0 i in
              if String.length raw > 0 && raw.[String.length raw - 1] = '\r' then
                String.sub raw 0 (String.length raw - 1)
              else raw
            in
            state.buffer <- String.sub state.buffer (i + 1) (String.length state.buffer - i - 1);
            match parse_line line with
            | Empty ->
                if String.length state.current_event.data > 0 then begin
                  let event = state.current_event in
                  state.current_event <- empty_event;
                  if is_done_event event then begin
                    state.done_ <- true;
                    None
                  end else
                    Some event
                end else
                  process_lines ()
            | Comment -> process_lines ()
            | Field (field, value) ->
                state.current_event <- apply_field state.current_event field value;
                process_lines ()
      in
      process_lines ()

  let is_done state = state.done_
end

let parse_chunk chunk =
  let state = Stream.create () in
  Stream.feed state chunk;
  let rec collect acc =
    match Stream.next_event state with
    | None -> List.rev acc
    | Some event -> collect (event :: acc)
  in
  collect []
