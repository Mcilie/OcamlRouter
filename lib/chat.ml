(** Chat request and response types *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(** Logit bias - maps token IDs to bias values *)
type logit_bias = (string * float) list

let yojson_of_logit_bias lb =
  `Assoc (List.map (fun (k, v) -> (k, `Float v)) lb)

let logit_bias_of_yojson = function
  | `Assoc pairs ->
    List.map (fun (k, v) ->
      match v with
      | `Float f -> (k, f)
      | `Int i -> (k, float_of_int i)
      | _ -> failwith "Invalid logit_bias value"
    ) pairs
  | _ -> failwith "Invalid logit_bias"

(** Metadata - custom key-value pairs *)
type metadata = (string * string) list

let yojson_of_metadata m =
  `Assoc (List.map (fun (k, v) -> (k, `String v)) m)

let metadata_of_yojson = function
  | `Assoc pairs ->
    List.map (fun (k, v) ->
      match v with
      | `String s -> (k, s)
      | _ -> failwith "Invalid metadata value"
    ) pairs
  | _ -> failwith "Invalid metadata"

(** Modalities for output *)
type modality = Text | Audio

let yojson_of_modality = function
  | Text -> `String "text"
  | Audio -> `String "audio"

let modality_of_yojson = function
  | `String "text" -> Text
  | `String "audio" -> Audio
  | _ -> Text

type request = {
  model : string option; [@yojson.option]
  models : string list option; [@yojson.option]
  messages : Message.t list;
  temperature : float option; [@yojson.option]
  top_p : float option; [@yojson.option]
  top_k : int option; [@yojson.option]
  max_tokens : int option; [@yojson.option]
  max_completion_tokens : int option; [@yojson.option]
  stream : bool option; [@yojson.option]
  stop : string list option; [@yojson.option]
  tools : Tool.tool list option; [@yojson.option]
  tool_choice : Tool.tool_choice option; [@yojson.option]
  reasoning : Reasoning.t option; [@yojson.option]
  response_format : Common.response_format option; [@yojson.option]
  frequency_penalty : float option; [@yojson.option]
  presence_penalty : float option; [@yojson.option]
  repetition_penalty : float option; [@yojson.option]
  seed : int option; [@yojson.option]
  user : string option; [@yojson.option]
  provider : Provider.preferences option; [@yojson.option]
  route : Provider.route option; [@yojson.option]
  (* New parameters *)
  logit_bias : logit_bias option; [@yojson.option]
  logprobs : bool option; [@yojson.option]
  top_logprobs : int option; [@yojson.option]
  session_id : string option; [@yojson.option]
  metadata : metadata option; [@yojson.option]
  modalities : modality list option; [@yojson.option]
}
[@@deriving yojson]

type choice = {
  index : int;
  message : Message.t;
  finish_reason : string option; [@yojson.option]
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

type response = {
  id : string;
  model : string;
  created : int;
  choices : choice list;
  usage : Common.usage option; [@yojson.option]
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

(** Create a minimal request with just messages *)
let make_request ?model ?models ?temperature ?top_p ?top_k ?max_tokens
    ?max_completion_tokens ?stream ?stop ?tools ?tool_choice ?reasoning
    ?response_format ?frequency_penalty ?presence_penalty ?repetition_penalty
    ?seed ?user ?provider ?route ?logit_bias ?logprobs ?top_logprobs
    ?session_id ?metadata ?modalities messages =
  {
    model;
    models;
    messages;
    temperature;
    top_p;
    top_k;
    max_tokens;
    max_completion_tokens;
    stream;
    stop;
    tools;
    tool_choice;
    reasoning;
    response_format;
    frequency_penalty;
    presence_penalty;
    repetition_penalty;
    seed;
    user;
    provider;
    route;
    logit_bias;
    logprobs;
    top_logprobs;
    session_id;
    metadata;
    modalities;
  }

(** Get the first choice's message content as string *)
let get_content response =
  match response.choices with
  | [] -> None
  | choice :: _ -> (
      match choice.message.content with
      | Some (Content.String_content s) -> Some s
      | Some (Content.Parts_content _) -> None
      | None -> None)

(** Get tool calls from the first choice *)
let get_tool_calls response =
  match response.choices with
  | [] -> None
  | choice :: _ -> choice.message.tool_calls
