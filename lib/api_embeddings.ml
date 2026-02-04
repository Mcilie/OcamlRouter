(** Embeddings API endpoints *)

(** Generate embeddings for input text *)
let generate ~sw ~env client ?encoding_format ?dimensions ?user ?input_type
    ~model ~input () =
  let request =
    Embeddings.make_request ?encoding_format ?dimensions ?user ?input_type
      ~model input
  in
  let body = Embeddings.yojson_of_request request in
  match Client.post ~sw ~env client ~path:"/embeddings" ~body with
  | Ok json -> (
      try Ok (Embeddings.response_of_yojson json)
      with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        Error (Errors.Parse_error (Printexc.to_string exn))
      | Failure msg ->
        Error (Errors.Parse_error msg))
  | Error e -> Error e

(** List available embedding models *)
let list_models ~sw ~env client =
  match Client.get ~sw ~env client ~path:"/embeddings/models" with
  | Ok json -> (
      try Ok (Models.list_response_of_yojson json)
      with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        Error (Errors.Parse_error (Printexc.to_string exn))
      | Failure msg ->
        Error (Errors.Parse_error msg))
  | Error e -> Error e
