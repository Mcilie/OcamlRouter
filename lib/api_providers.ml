(** Providers API endpoints *)

(** List all providers *)
let list ~sw ~env client =
  match Client.get ~sw ~env client ~path:"/providers" with
  | Ok json -> (
      try Ok (Providers.list_response_of_yojson json)
      with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        Error (Errors.Parse_error (Printexc.to_string exn))
      | Failure msg ->
        Error (Errors.Parse_error msg))
  | Error e -> Error e
