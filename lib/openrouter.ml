(** OCaml SDK for the OpenRouter API

    This library provides a type-safe interface to the OpenRouter API,
    supporting chat completions, streaming, tool calls, and more.

    {1 Quick Start}

    {[
      let () =
        Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
        let client = Openrouter.create ~api_key:"sk-..." () in
        let messages = [
          Openrouter.Message.system "You are a helpful assistant.";
          Openrouter.Message.user "Hello!";
        ] in
        match Openrouter.Chat.send ~sw ~env client ~model:"openai/gpt-4" ~messages () with
        | Ok response ->
          (match Openrouter.Chat.Response.get_content response with
           | Some content -> print_endline content
           | None -> ())
        | Error e ->
          Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e)
    ]}
*)

let version = "0.1.0"

(** {1 Modules} *)

module Client = Client
module Config = Config
module Auth = Auth
module Errors = Errors
module Retry = Retry
module Sse = Sse
module Env_check = Env_check

(** {2 Types} *)

module Common = Common
module Content = Content
module Message = Message
module Tool = Tool
module Reasoning = Reasoning
module Provider = Provider
module Streaming = Streaming
module Pipeline = Pipeline

module Chat = struct
  include Chat

  (** {1 API Functions} *)

  let send = Api_chat.send
  let send_streaming = Api_chat.send_streaming
  let iter_stream = Api_chat.iter_stream
  let fold_stream = Api_chat.fold_stream
  let collect_content = Api_chat.collect_content
  let collect_chunks = Api_chat.collect_chunks
end

module Models = struct
  include Models

  (** {1 API Functions} *)

  let list = Api_models.list
  let count = Api_models.count
  let endpoints = Api_models.endpoints
  let find_by_id = Api_models.find_by_id
  let filter_by_input_modality = Api_models.filter_by_input_modality
  let filter_by_output_modality = Api_models.filter_by_output_modality
  let filter_by_parameter = Api_models.filter_by_parameter
end

module Account = struct
  include Account

  (** {1 API Functions} *)

  let get_credits = Api_account.get_credits
  let get_generation = Api_account.get_generation
end

module Providers = struct
  include Providers

  (** {1 API Functions} *)

  let list = Api_providers.list
end

module Completions = struct
  include Completions

  (** {1 API Functions} *)

  let generate = Api_completions.generate
end

module Embeddings = struct
  include Embeddings

  (** {1 API Functions} *)

  let generate = Api_embeddings.generate
  let list_models = Api_embeddings.list_models
end

(** {1 Convenience Functions} *)

(** Create a new client with the given API key *)
let create ?base_url ?timeout ?app_name ?app_url ?retry_config ~api_key () =
  let config =
    Config.create ?base_url ?timeout ?app_name ?app_url ()
  in
  Client.create ~config ?retry_config ~api_key ()

(** Create a client from the OPENROUTER_API_KEY environment variable *)
let from_env ?base_url ?timeout ?app_name ?app_url ?retry_config () =
  let config =
    Config.create ?base_url ?timeout ?app_name ?app_url ()
  in
  Client.from_env ~config ?retry_config ()
