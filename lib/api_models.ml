(** Models API endpoints *)

(** List all available models *)
let list ~sw ~env client =
  match Client.get ~sw ~env client ~path:"/models" with
  | Ok json -> (
      try Ok (Models.list_response_of_yojson json)
      with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        Error (Errors.Parse_error (Printexc.to_string exn)))
  | Error e -> Error e

(** Get the count of available models *)
let count ~sw ~env client =
  match Client.get ~sw ~env client ~path:"/models/count" with
  | Ok json -> (
      try Ok (Models.count_response_of_yojson json)
      with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        Error (Errors.Parse_error (Printexc.to_string exn)))
  | Error e -> Error e

(** Get endpoints for a specific model *)
let endpoints ~sw ~env client ~author ~slug =
  let path = Printf.sprintf "/models/%s/%s/endpoints" author slug in
  match Client.get ~sw ~env client ~path with
  | Ok json -> (
      try Ok (Models.endpoints_response_of_yojson json)
      with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        Error (Errors.Parse_error (Printexc.to_string exn)))
  | Error e -> Error e

(** Find a model by ID *)
let find_by_id models id =
  List.find_opt (fun (m : Models.t) -> m.id = id) models

(** Filter models by input modality *)
let filter_by_input_modality models modality =
  List.filter (fun (m : Models.t) ->
    List.mem modality m.architecture.input_modalities
  ) models

(** Filter models by output modality *)
let filter_by_output_modality models modality =
  List.filter (fun (m : Models.t) ->
    List.mem modality m.architecture.output_modalities
  ) models

(** Get models that support a specific parameter *)
let filter_by_parameter models param =
  List.filter (fun (m : Models.t) ->
    List.mem param m.supported_parameters
  ) models
