(** HTTP client wrapper using curl subprocess for HTTPS support *)

type t = {
  config : Config.t;
  auth : Auth.t;
  retry_config : Retry.config;
}

let create ?(config = Config.default) ?(retry_config = Retry.default) ~api_key () =
  { config; auth = Auth.create api_key; retry_config }

let from_env ?config ?retry_config () =
  match Auth.from_env () with
  | Some auth ->
      Some
        {
          config = Option.value config ~default:Config.default;
          auth;
          retry_config = Option.value retry_config ~default:Retry.default;
        }
  | None -> None

let make_headers t =
  let base_headers =
    [
      Auth.authorization_header t.auth;
      ("Content-Type", "application/json");
    ]
  in
  let extra = Config.extra_headers t.config in
  base_headers @ extra

(* Retry waitpid on EINTR *)
let rec waitpid_retry pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_retry pid

(* Use curl for HTTP GET requests *)
let curl_get t ~url =
  let headers = make_headers t in
  let header_args = List.concat_map (fun (k, v) ->
    ["-H"; Printf.sprintf "%s: %s" k v]
  ) headers in
  let args = Array.of_list ([
    "curl"; "-s"; "-S";
    "-X"; "GET";
    "-w"; "\n%{http_code}";
  ] @ header_args @ [url]) in

  (* Create a pipe for reading stdout *)
  let read_fd, write_fd = Unix.pipe () in
  let pid = Unix.create_process "curl" args Unix.stdin write_fd Unix.stderr in
  Unix.close write_fd;

  (* Read all output *)
  let ic = Unix.in_channel_of_descr read_fd in
  let buf = Buffer.create 4096 in
  (try
    while true do
      Buffer.add_channel buf ic 1
    done
  with End_of_file -> ());
  close_in ic;

  (* Wait for process *)
  let _ = waitpid_retry pid in

  let output = Buffer.contents buf in
  (* Parse response: body followed by newline and status code *)
  let lines = String.split_on_char '\n' output in
  let rec find_status_and_body acc = function
    | [] -> (500, String.concat "\n" (List.rev acc))
    | [last] ->
        (match int_of_string_opt (String.trim last) with
         | Some code -> (code, String.concat "\n" (List.rev acc))
         | None -> (500, String.concat "\n" (List.rev (last :: acc))))
    | line :: rest -> find_status_and_body (line :: acc) rest
  in
  find_status_and_body [] lines

(* Use curl for HTTP POST requests via Unix process *)
let curl_post t ~url ~body_str =
  let headers = make_headers t in
  let header_args = List.concat_map (fun (k, v) ->
    ["-H"; Printf.sprintf "%s: %s" k v]
  ) headers in
  let args = Array.of_list ([
    "curl"; "-s"; "-S";
    "-X"; "POST";
    "-w"; "\n%{http_code}";
  ] @ header_args @ [
    "-d"; body_str;
    url
  ]) in

  (* Create a pipe for reading stdout *)
  let read_fd, write_fd = Unix.pipe () in
  let pid = Unix.create_process "curl" args Unix.stdin write_fd Unix.stderr in
  Unix.close write_fd;

  (* Read all output *)
  let ic = Unix.in_channel_of_descr read_fd in
  let buf = Buffer.create 4096 in
  (try
    while true do
      Buffer.add_channel buf ic 1
    done
  with End_of_file -> ());
  close_in ic;

  (* Wait for process *)
  let _ = waitpid_retry pid in

  let output = Buffer.contents buf in
  (* Parse response: body followed by newline and status code *)
  let lines = String.split_on_char '\n' output in
  let rec find_status_and_body acc = function
    | [] -> (500, String.concat "\n" (List.rev acc))
    | [last] ->
        (match int_of_string_opt (String.trim last) with
         | Some code -> (code, String.concat "\n" (List.rev acc))
         | None -> (500, String.concat "\n" (List.rev (last :: acc))))
    | line :: rest -> find_status_and_body (line :: acc) rest
  in
  find_status_and_body [] lines

let get ~sw:_ ~env:_ t ~path =
  let url = t.config.base_url ^ path in
  try
    let status, body_content = curl_get t ~url in
    if status >= 200 && status < 300 then
      match Yojson.Safe.from_string body_content with
      | json -> Ok json
      | exception exn -> Error (Errors.Parse_error (Printexc.to_string exn))
    else Error (Errors.from_status ~status ~body:body_content)
  with exn ->
    Error (Errors.Network_error (Printexc.to_string exn))

let post ~sw:_ ~env:_ t ~path ~body =
  let url = t.config.base_url ^ path in
  let body_str = Yojson.Safe.to_string body in
  try
    let status, body_content = curl_post t ~url ~body_str in
    if status >= 200 && status < 300 then
      match Yojson.Safe.from_string body_content with
      | json -> Ok json
      | exception exn -> Error (Errors.Parse_error (Printexc.to_string exn))
    else Error (Errors.from_status ~status ~body:body_content)
  with exn ->
    Error (Errors.Network_error (Printexc.to_string exn))

(* Streaming implementation *)
type stream_reader = {
  ic : in_channel;
  pid : int;
  sse_state : Sse.Stream.state;
  mutable finished : bool;
}

let post_stream ~sw:_ ~env:_ t ~path ~body =
  let url = t.config.base_url ^ path in
  let body_str = Yojson.Safe.to_string body in
  let headers = make_headers t in
  let header_args = List.concat_map (fun (k, v) ->
    ["-H"; Printf.sprintf "%s: %s" k v]
  ) headers in
  let args = Array.of_list ([
    "curl"; "-s"; "-S"; "-N";
    "-X"; "POST";
  ] @ header_args @ [
    "-d"; body_str;
    url
  ]) in

  try
    let read_fd, write_fd = Unix.pipe () in
    let pid = Unix.create_process "curl" args Unix.stdin write_fd Unix.stderr in
    Unix.close write_fd;
    let ic = Unix.in_channel_of_descr read_fd in
    let sse_state = Sse.Stream.create () in
    Ok { ic; pid; sse_state; finished = false }
  with exn ->
    Error (Errors.Network_error (Printexc.to_string exn))

let stream_next reader =
  if reader.finished then None
  else
    let rec try_get_event () =
      match Sse.Stream.next_event reader.sse_state with
      | Some event -> Some event
      | None ->
          if Sse.Stream.is_done reader.sse_state then begin
            reader.finished <- true;
            close_in_noerr reader.ic;
            let _ = waitpid_retry reader.pid in
            None
          end else begin
            try
              let line = input_line reader.ic in
              Sse.Stream.feed reader.sse_state (line ^ "\n");
              try_get_event ()
            with End_of_file ->
              reader.finished <- true;
              close_in_noerr reader.ic;
              let _ = waitpid_retry reader.pid in
              None
          end
    in
    try_get_event ()

let stream_iter f reader =
  let rec loop () =
    match stream_next reader with
    | None -> ()
    | Some event ->
        f event;
        loop ()
  in
  loop ()

let stream_fold f init reader =
  let rec loop acc =
    match stream_next reader with
    | None -> acc
    | Some event -> loop (f acc event)
  in
  loop init

let api_key t = Auth.api_key t.auth
let base_url t = t.config.base_url
