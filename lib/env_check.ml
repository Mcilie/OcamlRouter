(** Environment checks for runtime requirements *)

exception Ulimit_too_high of int

let max_safe_open_files = 1048576 (* 1M is reasonable upper bound *)

let get_open_files_limit () =
  try
    let ic = Unix.open_process_in "ulimit -n" in
    let line = input_line ic in
    let _ = Unix.close_process_in ic in
    if String.trim line = "unlimited" then
      max_int
    else
      int_of_string (String.trim line)
  with _ ->
    (* If we can't determine, assume it's fine *)
    max_safe_open_files

let check_ulimit () =
  let limit = get_open_files_limit () in
  if limit > max_safe_open_files then
    Error (Printf.sprintf
      "File descriptor limit too high (%s). Eio's iomux can't handle unlimited or very large ulimits.\n\
       Fix: Run 'ulimit -n 10240' before starting your application, or add it to your shell profile."
      (if limit = max_int then "unlimited" else string_of_int limit))
  else
    Ok limit

let check_ulimit_exn () =
  match check_ulimit () with
  | Ok _ -> ()
  | Error msg -> failwith msg

let ensure_safe_environment () =
  check_ulimit_exn ()
