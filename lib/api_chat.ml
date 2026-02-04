(** Chat completions API *)

module Types = struct
  module Message = Message
  module Chat = Chat
  module Tool = Tool
  module Reasoning = Reasoning
  module Streaming = Streaming
  module Content = Content
  module Provider = Provider
  module Common = Common
end

let send ~sw ~env client ?model ?models ?temperature ?top_p ?top_k ?max_tokens
    ?max_completion_tokens ?stop ?tools ?tool_choice ?reasoning ?response_format
    ?frequency_penalty ?presence_penalty ?repetition_penalty ?seed ?user
    ?provider ?route ?logit_bias ?logprobs ?top_logprobs ?session_id ?metadata
    ?modalities ~messages () =
  let request =
    Chat.make_request ?model ?models ?temperature ?top_p ?top_k ?max_tokens
      ?max_completion_tokens ~stream:false ?stop ?tools ?tool_choice
      ?reasoning ?response_format ?frequency_penalty ?presence_penalty
      ?repetition_penalty ?seed ?user ?provider ?route ?logit_bias ?logprobs
      ?top_logprobs ?session_id ?metadata ?modalities messages
  in
  let body = Chat.yojson_of_request request in
  match Client.post ~sw ~env client ~path:"/chat/completions" ~body with
  | Ok json -> (
      try Ok (Chat.response_of_yojson json)
      with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        Error (Errors.Parse_error (Printexc.to_string exn)))
  | Error e -> Error e

let send_streaming ~sw ~env client ?model ?models ?temperature ?top_p ?top_k
    ?max_tokens ?max_completion_tokens ?stop ?tools ?tool_choice ?reasoning
    ?response_format ?frequency_penalty ?presence_penalty ?repetition_penalty
    ?seed ?user ?provider ?route ?logit_bias ?logprobs ?top_logprobs ?session_id
    ?metadata ?modalities ~messages () =
  let request =
    Chat.make_request ?model ?models ?temperature ?top_p ?top_k ?max_tokens
      ?max_completion_tokens ~stream:true ?stop ?tools ?tool_choice
      ?reasoning ?response_format ?frequency_penalty ?presence_penalty
      ?repetition_penalty ?seed ?user ?provider ?route ?logit_bias ?logprobs
      ?top_logprobs ?session_id ?metadata ?modalities messages
  in
  let body = Chat.yojson_of_request request in
  match Client.post_stream ~sw ~env client ~path:"/chat/completions" ~body with
  | Ok reader ->
      let parse_chunk event =
        try
          let json = Yojson.Safe.from_string event.Sse.data in
          Some (Streaming.chunk_of_yojson json)
        with _ -> None
      in
      Ok (reader, parse_chunk)
  | Error e -> Error e

let iter_stream f (reader, parse_chunk) =
  Client.stream_iter
    (fun event ->
      match parse_chunk event with
      | Some chunk -> f chunk
      | None -> ())
    reader

let fold_stream f init (reader, parse_chunk) =
  Client.stream_fold
    (fun acc event ->
      match parse_chunk event with
      | Some chunk -> f acc chunk
      | None -> acc)
    init reader

(** Collect all text content from a stream *)
let collect_content stream =
  let buf = Buffer.create 1024 in
  iter_stream
    (fun chunk ->
      match Streaming.get_content chunk with
      | Some text -> Buffer.add_string buf text
      | None -> ())
    stream;
  Buffer.contents buf

(** Collect all chunks from a stream into a list *)
let collect_chunks stream =
  List.rev (fold_stream (fun acc chunk -> chunk :: acc) [] stream)
