(** Composable pipeline builder for chat completions

    This module provides a fluent, pipe-friendly API for building chat requests.

    {1 Example}

    {[
      open Openrouter.Pipeline

      let result =
        "What is the capital of France?"
        |> prompt
        |> model "openai/gpt-5"
        |> temperature 0.7
        |> run ~sw ~env client
    ]}
*)

(** The pipeline state - accumulates configuration as you build *)
type t = {
  messages : Message.t list;
  model : string option;
  models : string list option;
  temperature : float option;
  top_p : float option;
  top_k : int option;
  max_tokens : int option;
  max_completion_tokens : int option;
  stop : string list option;
  tools : Tool.tool list option;
  tool_choice : Tool.tool_choice option;
  reasoning : Reasoning.t option;
  response_format : Common.response_format option;
  frequency_penalty : float option;
  presence_penalty : float option;
  repetition_penalty : float option;
  seed : int option;
  user : string option;
  provider : Provider.preferences option;
  route : Provider.route option;
  logit_bias : Chat.logit_bias option;
  logprobs : bool option;
  top_logprobs : int option;
  session_id : string option;
  metadata : Chat.metadata option;
  modalities : Chat.modality list option;
}

(** Create an empty pipeline *)
let empty = {
  messages = [];
  model = None;
  models = None;
  temperature = None;
  top_p = None;
  top_k = None;
  max_tokens = None;
  max_completion_tokens = None;
  stop = None;
  tools = None;
  tool_choice = None;
  reasoning = None;
  response_format = None;
  frequency_penalty = None;
  presence_penalty = None;
  repetition_penalty = None;
  seed = None;
  user = None;
  provider = None;
  route = None;
  logit_bias = None;
  logprobs = None;
  top_logprobs = None;
  session_id = None;
  metadata = None;
  modalities = None;
}

(** {1 Starting a Pipeline} *)

(** Start a pipeline with a user prompt *)
let prompt text =
  { empty with messages = [Message.user text] }

(** Start a pipeline from existing messages *)
let messages msgs =
  { empty with messages = msgs }

(** {1 Adding Messages} *)

(** Add a system message *)
let system text t =
  { t with messages = Message.system text :: t.messages }

(** Add a user message *)
let user text t =
  { t with messages = t.messages @ [Message.user text] }

(** Add a user message with multimodal content parts *)
let user_parts parts t =
  { t with messages = t.messages @ [Message.user_with_parts parts] }

(** Add an assistant message *)
let assistant text t =
  { t with messages = t.messages @ [Message.assistant text] }

(** Add a tool result message *)
let tool_result ~tool_call_id content t =
  { t with messages = t.messages @ [Message.tool ~tool_call_id content] }

(** {1 Model Selection} *)

(** Set the model to use *)
let model m t =
  { t with model = Some m }

(** Set multiple models for fallback *)
let models ms t =
  { t with models = Some ms }

(** {1 Generation Parameters} *)

(** Set temperature (0.0 to 2.0) *)
let temperature temp t =
  { t with temperature = Some temp }

(** Set top_p (nucleus sampling) *)
let top_p p t =
  { t with top_p = Some p }

(** Set top_k *)
let top_k k t =
  { t with top_k = Some k }

(** Set max tokens to generate *)
let max_tokens n t =
  { t with max_tokens = Some n }

(** Set max completion tokens (for newer models) *)
let max_completion_tokens n t =
  { t with max_completion_tokens = Some n }

(** Set stop sequences *)
let stop seqs t =
  { t with stop = Some seqs }

(** Set frequency penalty *)
let frequency_penalty p t =
  { t with frequency_penalty = Some p }

(** Set presence penalty *)
let presence_penalty p t =
  { t with presence_penalty = Some p }

(** Set repetition penalty *)
let repetition_penalty p t =
  { t with repetition_penalty = Some p }

(** Set seed for reproducibility *)
let seed s t =
  { t with seed = Some s }

(** Set user identifier *)
let with_user u t =
  { t with user = Some u }

(** {1 Response Formatting} *)

(** Enable JSON mode with a schema for structured output *)
let json_mode ~name ?strict schema t =
  { t with response_format = Some (Common.Json_schema_format { name; schema; strict }) }

(** Alias for json_mode *)
let json_schema = json_mode

(** Set text response format *)
let text_format t =
  { t with response_format = Some Common.Text_format }

(** {1 Tools} *)

(** Add tools to the request *)
let tools ts t =
  { t with tools = Some ts }

(** Add a single tool *)
let add_tool tool t =
  let current = Option.value t.tools ~default:[] in
  { t with tools = Some (current @ [tool]) }

(** Set tool choice *)
let tool_choice choice t =
  { t with tool_choice = Some choice }

(** Force a specific tool to be called *)
let force_tool name t =
  { t with tool_choice = Some (Tool.Function_choice name) }

(** {1 Reasoning} *)

(** Enable reasoning with specified effort *)
let reasoning effort t =
  { t with reasoning = Some { Reasoning.effort = Some effort; max_tokens = None } }

(** Enable reasoning with max tokens *)
let reasoning_tokens max_tokens t =
  { t with reasoning = Some { Reasoning.effort = None; max_tokens = Some max_tokens } }

(** {1 Provider Preferences} *)

(** Set provider preferences *)
let provider prefs t =
  { t with provider = Some prefs }

(** Set routing mode *)
let route r t =
  { t with route = Some r }

(** {1 Advanced Parameters} *)

(** Set logit bias *)
let logit_bias lb t =
  { t with logit_bias = Some lb }

(** Enable logprobs *)
let logprobs ?(top_logprobs = 5) () t =
  { t with logprobs = Some true; top_logprobs = Some top_logprobs }

(** Set session ID for grouping requests *)
let session_id sid t =
  { t with session_id = Some sid }

(** Set metadata *)
let metadata m t =
  { t with metadata = Some m }

(** Set output modalities *)
let modalities ms t =
  { t with modalities = Some ms }

(** {1 Execution} *)

(** Execute the pipeline and return the response *)
let run ~sw ~env client t =
  Api_chat.send ~sw ~env client
    ?model:t.model
    ?models:t.models
    ?temperature:t.temperature
    ?top_p:t.top_p
    ?top_k:t.top_k
    ?max_tokens:t.max_tokens
    ?max_completion_tokens:t.max_completion_tokens
    ?stop:t.stop
    ?tools:t.tools
    ?tool_choice:t.tool_choice
    ?reasoning:t.reasoning
    ?response_format:t.response_format
    ?frequency_penalty:t.frequency_penalty
    ?presence_penalty:t.presence_penalty
    ?repetition_penalty:t.repetition_penalty
    ?seed:t.seed
    ?user:t.user
    ?provider:t.provider
    ?route:t.route
    ?logit_bias:t.logit_bias
    ?logprobs:t.logprobs
    ?top_logprobs:t.top_logprobs
    ?session_id:t.session_id
    ?metadata:t.metadata
    ?modalities:t.modalities
    ~messages:t.messages
    ()

(** Execute the pipeline with streaming *)
let run_stream ~sw ~env client t =
  Api_chat.send_streaming ~sw ~env client
    ?model:t.model
    ?models:t.models
    ?temperature:t.temperature
    ?top_p:t.top_p
    ?top_k:t.top_k
    ?max_tokens:t.max_tokens
    ?max_completion_tokens:t.max_completion_tokens
    ?stop:t.stop
    ?tools:t.tools
    ?tool_choice:t.tool_choice
    ?reasoning:t.reasoning
    ?response_format:t.response_format
    ?frequency_penalty:t.frequency_penalty
    ?presence_penalty:t.presence_penalty
    ?repetition_penalty:t.repetition_penalty
    ?seed:t.seed
    ?user:t.user
    ?provider:t.provider
    ?route:t.route
    ?logit_bias:t.logit_bias
    ?logprobs:t.logprobs
    ?top_logprobs:t.top_logprobs
    ?session_id:t.session_id
    ?metadata:t.metadata
    ?modalities:t.modalities
    ~messages:t.messages
    ()

(** {1 Result Helpers} *)

(** Extract text content from a successful response *)
let get_content = function
  | Ok response -> Chat.get_content response
  | Error _ -> None

(** Extract tool calls from a successful response *)
let get_tool_calls = function
  | Ok response -> Chat.get_tool_calls response
  | Error _ -> None

(** Map over a successful response *)
let map f = function
  | Ok response -> Ok (f response)
  | Error e -> Error e

(** Map to extract content directly *)
let map_content result =
  map (fun r -> Chat.get_content r) result

(** {1 Infix Operators} *)

module Infix = struct
  (** Alias for adding a user message *)
  let ( +> ) t text = user text t

  (** Alias for adding a system message (prepends) *)
  let ( +< ) t text = system text t
end
