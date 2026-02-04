(** Account API endpoints *)

(** Get credits information *)
let get_credits ~sw ~env client =
  match Client.get ~sw ~env client ~path:"/credits" with
  | Ok json -> (
      try Ok (Account.credits_response_of_yojson json)
      with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        Error (Errors.Parse_error (Printexc.to_string exn))
      | Failure msg ->
        Error (Errors.Parse_error msg))
  | Error e -> Error e

(** Get generation metadata by ID *)
let get_generation ~sw ~env client ~id =
  let path = Printf.sprintf "/generation?id=%s" (Uri.pct_encode id) in
  match Client.get ~sw ~env client ~path with
  | Ok json -> (
      try Ok (Account.generation_response_of_yojson json)
      with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        Error (Errors.Parse_error (Printexc.to_string exn))
      | Failure msg ->
        Error (Errors.Parse_error msg))
  | Error e -> Error e
