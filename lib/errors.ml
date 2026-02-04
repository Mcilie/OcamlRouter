(** Error types and handling *)

type error_data = {
  message : string;
  code : int option;
  metadata : Yojson.Safe.t option;
}

type t =
  | Bad_request of error_data
  | Unauthorized of error_data
  | Payment_required of error_data
  | Forbidden of error_data
  | Not_found of error_data
  | Too_many_requests of error_data
  | Internal_server of error_data
  | Bad_gateway of error_data
  | Service_unavailable of error_data
  | Gateway_timeout of error_data
  | Network_error of string
  | Parse_error of string
  | Unknown of int * string

let status_code = function
  | Bad_request _ -> 400
  | Unauthorized _ -> 401
  | Payment_required _ -> 402
  | Forbidden _ -> 403
  | Not_found _ -> 404
  | Too_many_requests _ -> 429
  | Internal_server _ -> 500
  | Bad_gateway _ -> 502
  | Service_unavailable _ -> 503
  | Gateway_timeout _ -> 504
  | Network_error _ -> 0
  | Parse_error _ -> 0
  | Unknown (code, _) -> code

let is_retryable = function
  | Too_many_requests _ -> true
  | Internal_server _ -> true
  | Bad_gateway _ -> true
  | Service_unavailable _ -> true
  | Gateway_timeout _ -> true
  | Network_error _ -> true
  | _ -> false

let from_status ~status ~body =
  let data =
    try
      let json = Yojson.Safe.from_string body in
      let open Yojson.Safe.Util in
      let error_obj =
        try json |> member "error" with _ -> json
      in
      let message =
        try error_obj |> member "message" |> to_string
        with _ -> body
      in
      let code =
        try Some (error_obj |> member "code" |> to_int)
        with _ -> None
      in
      let metadata =
        try
          let m = error_obj |> member "metadata" in
          if m = `Null then None else Some m
        with _ -> None
      in
      { message; code; metadata }
    with _ -> { message = body; code = None; metadata = None }
  in
  match status with
  | 400 -> Bad_request data
  | 401 -> Unauthorized data
  | 402 -> Payment_required data
  | 403 -> Forbidden data
  | 404 -> Not_found data
  | 429 -> Too_many_requests data
  | 500 -> Internal_server data
  | 502 -> Bad_gateway data
  | 503 -> Service_unavailable data
  | 504 -> Gateway_timeout data
  | code -> Unknown (code, body)

let to_string = function
  | Bad_request data -> Printf.sprintf "Bad Request (400): %s" data.message
  | Unauthorized data -> Printf.sprintf "Unauthorized (401): %s" data.message
  | Payment_required data -> Printf.sprintf "Payment Required (402): %s" data.message
  | Forbidden data -> Printf.sprintf "Forbidden (403): %s" data.message
  | Not_found data -> Printf.sprintf "Not Found (404): %s" data.message
  | Too_many_requests data ->
      Printf.sprintf "Too Many Requests (429): %s" data.message
  | Internal_server data ->
      Printf.sprintf "Internal Server Error (500): %s" data.message
  | Bad_gateway data -> Printf.sprintf "Bad Gateway (502): %s" data.message
  | Service_unavailable data ->
      Printf.sprintf "Service Unavailable (503): %s" data.message
  | Gateway_timeout data -> Printf.sprintf "Gateway Timeout (504): %s" data.message
  | Network_error msg -> Printf.sprintf "Network Error: %s" msg
  | Parse_error msg -> Printf.sprintf "Parse Error: %s" msg
  | Unknown (code, msg) -> Printf.sprintf "Unknown Error (%d): %s" code msg

let pp fmt error = Format.fprintf fmt "%s" (to_string error)

exception Openrouter_error of t

let raise_error e = raise (Openrouter_error e)
