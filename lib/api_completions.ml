(** Completions API endpoints *)

(** Generate text completion *)
let generate ~sw ~env client ?model ?models ?best_of ?echo ?frequency_penalty
    ?logprobs ?max_tokens ?n ?presence_penalty ?seed ?stop ?suffix ?temperature
    ?top_p ?user ~prompt () =
  let request =
    Completions.make_request ?model ?models ?best_of ?echo ?frequency_penalty
      ?logprobs ?max_tokens ?n ?presence_penalty ?seed ?stop ~stream:false
      ?suffix ?temperature ?top_p ?user prompt
  in
  let body = Completions.yojson_of_request request in
  match Client.post ~sw ~env client ~path:"/completions" ~body with
  | Ok json -> (
      try Ok (Completions.response_of_yojson json)
      with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        Error (Errors.Parse_error (Printexc.to_string exn))
      | Failure msg ->
        Error (Errors.Parse_error msg))
  | Error e -> Error e
