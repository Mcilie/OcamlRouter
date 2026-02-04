(** Model information types *)

(* Helper to handle JSON numbers that can be int or float *)
let to_number_option json =
  match json with
  | `Float f -> Some f
  | `Int i -> Some (float_of_int i)
  | `Null -> None
  | _ -> None

(** Pricing information for a model *)
type pricing = {
  prompt : string;
  completion : string;
  request : string option;
  image : string option;
  image_token : string option;
  image_output : string option;
  audio : string option;
  audio_output : string option;
  input_audio_cache : string option;
  web_search : string option;
  internal_reasoning : string option;
  input_cache_read : string option;
  input_cache_write : string option;
  discount : float option;
}

let pricing_of_yojson json =
  let open Yojson.Safe.Util in
  {
    prompt = json |> member "prompt" |> to_string;
    completion = json |> member "completion" |> to_string;
    request = json |> member "request" |> to_string_option;
    image = json |> member "image" |> to_string_option;
    image_token = json |> member "image_token" |> to_string_option;
    image_output = json |> member "image_output" |> to_string_option;
    audio = json |> member "audio" |> to_string_option;
    audio_output = json |> member "audio_output" |> to_string_option;
    input_audio_cache = json |> member "input_audio_cache" |> to_string_option;
    web_search = json |> member "web_search" |> to_string_option;
    internal_reasoning = json |> member "internal_reasoning" |> to_string_option;
    input_cache_read = json |> member "input_cache_read" |> to_string_option;
    input_cache_write = json |> member "input_cache_write" |> to_string_option;
    discount = json |> member "discount" |> to_number_option;
  }

let yojson_of_pricing p =
  let fields = [
    ("prompt", `String p.prompt);
    ("completion", `String p.completion);
  ] in
  let add_opt name v fields = match v with Some s -> (name, `String s) :: fields | None -> fields in
  let add_opt_float name v fields = match v with Some f -> (name, `Float f) :: fields | None -> fields in
  let fields = add_opt "request" p.request fields in
  let fields = add_opt "image" p.image fields in
  let fields = add_opt "image_token" p.image_token fields in
  let fields = add_opt "image_output" p.image_output fields in
  let fields = add_opt "audio" p.audio fields in
  let fields = add_opt "audio_output" p.audio_output fields in
  let fields = add_opt "input_audio_cache" p.input_audio_cache fields in
  let fields = add_opt "web_search" p.web_search fields in
  let fields = add_opt "internal_reasoning" p.internal_reasoning fields in
  let fields = add_opt "input_cache_read" p.input_cache_read fields in
  let fields = add_opt "input_cache_write" p.input_cache_write fields in
  let fields = add_opt_float "discount" p.discount fields in
  `Assoc fields

(** Model architecture information *)
type architecture = {
  modality : string option;
  input_modalities : string list;
  output_modalities : string list;
  tokenizer : string option;
  instruct_type : string option;
}

let architecture_of_yojson json =
  let open Yojson.Safe.Util in
  {
    modality = json |> member "modality" |> to_string_option;
    input_modalities = json |> member "input_modalities" |> to_list |> List.map to_string;
    output_modalities = json |> member "output_modalities" |> to_list |> List.map to_string;
    tokenizer = json |> member "tokenizer" |> to_string_option;
    instruct_type = json |> member "instruct_type" |> to_string_option;
  }

let yojson_of_architecture a =
  let fields = [
    ("input_modalities", `List (List.map (fun s -> `String s) a.input_modalities));
    ("output_modalities", `List (List.map (fun s -> `String s) a.output_modalities));
  ] in
  let fields = match a.modality with Some m -> ("modality", `String m) :: fields | None -> ("modality", `Null) :: fields in
  let fields = match a.tokenizer with Some t -> ("tokenizer", `String t) :: fields | None -> fields in
  let fields = match a.instruct_type with Some i -> ("instruct_type", `String i) :: fields | None -> ("instruct_type", `Null) :: fields in
  `Assoc fields

(** Top provider information *)
type top_provider = {
  is_moderated : bool;
  context_length : int option;
  max_completion_tokens : int option;
}

let top_provider_of_yojson json =
  let open Yojson.Safe.Util in
  {
    is_moderated = json |> member "is_moderated" |> to_bool;
    context_length = json |> member "context_length" |> to_int_option;
    max_completion_tokens = json |> member "max_completion_tokens" |> to_int_option;
  }

let yojson_of_top_provider tp =
  let fields = [("is_moderated", `Bool tp.is_moderated)] in
  let fields = match tp.context_length with Some c -> ("context_length", `Int c) :: fields | None -> ("context_length", `Null) :: fields in
  let fields = match tp.max_completion_tokens with Some m -> ("max_completion_tokens", `Int m) :: fields | None -> ("max_completion_tokens", `Null) :: fields in
  `Assoc fields

(** Per-request limits *)
type per_request_limits = {
  prompt_tokens : int option;
  completion_tokens : int option;
}

let per_request_limits_of_yojson json =
  let open Yojson.Safe.Util in
  {
    prompt_tokens = json |> member "prompt_tokens" |> to_int_option;
    completion_tokens = json |> member "completion_tokens" |> to_int_option;
  }

let yojson_of_per_request_limits prl =
  let fields = [] in
  let fields = match prl.prompt_tokens with Some p -> ("prompt_tokens", `Int p) :: fields | None -> fields in
  let fields = match prl.completion_tokens with Some c -> ("completion_tokens", `Int c) :: fields | None -> fields in
  `Assoc fields

(** Default parameters for a model *)
type default_parameters = {
  temperature : float option;
  top_p : float option;
  top_k : int option;
  frequency_penalty : float option;
  presence_penalty : float option;
  repetition_penalty : float option;
}

let default_parameters_of_yojson json =
  let open Yojson.Safe.Util in
  {
    temperature = json |> member "temperature" |> to_number_option;
    top_p = json |> member "top_p" |> to_number_option;
    top_k = json |> member "top_k" |> to_int_option;
    frequency_penalty = json |> member "frequency_penalty" |> to_number_option;
    presence_penalty = json |> member "presence_penalty" |> to_number_option;
    repetition_penalty = json |> member "repetition_penalty" |> to_number_option;
  }

let yojson_of_default_parameters dp =
  let fields = [] in
  let add_opt_float name v fields = match v with Some f -> (name, `Float f) :: fields | None -> fields in
  let add_opt_int name v fields = match v with Some i -> (name, `Int i) :: fields | None -> fields in
  let fields = add_opt_float "temperature" dp.temperature fields in
  let fields = add_opt_float "top_p" dp.top_p fields in
  let fields = add_opt_int "top_k" dp.top_k fields in
  let fields = add_opt_float "frequency_penalty" dp.frequency_penalty fields in
  let fields = add_opt_float "presence_penalty" dp.presence_penalty fields in
  let fields = add_opt_float "repetition_penalty" dp.repetition_penalty fields in
  `Assoc fields

(** Information about an AI model available on OpenRouter *)
type t = {
  id : string;
  canonical_slug : string;
  name : string;
  created : int;
  pricing : pricing;
  context_length : int option;
  architecture : architecture;
  top_provider : top_provider;
  per_request_limits : per_request_limits option;
  supported_parameters : string list;
  default_parameters : default_parameters option;
  hugging_face_id : string option;
  description : string option;
  expiration_date : string option;
}

let t_of_yojson json =
  let open Yojson.Safe.Util in
  {
    id = json |> member "id" |> to_string;
    canonical_slug = json |> member "canonical_slug" |> to_string;
    name = json |> member "name" |> to_string;
    created = json |> member "created" |> to_int;
    pricing = json |> member "pricing" |> pricing_of_yojson;
    context_length = json |> member "context_length" |> to_int_option;
    architecture = json |> member "architecture" |> architecture_of_yojson;
    top_provider = json |> member "top_provider" |> top_provider_of_yojson;
    per_request_limits = (match json |> member "per_request_limits" with
      | `Null -> None
      | j -> Some (per_request_limits_of_yojson j));
    supported_parameters = json |> member "supported_parameters" |> to_list |> List.map to_string;
    default_parameters = (match json |> member "default_parameters" with
      | `Null -> None
      | j -> Some (default_parameters_of_yojson j));
    hugging_face_id = json |> member "hugging_face_id" |> to_string_option;
    description = json |> member "description" |> to_string_option;
    expiration_date = json |> member "expiration_date" |> to_string_option;
  }

let yojson_of_t m =
  let fields = [
    ("id", `String m.id);
    ("canonical_slug", `String m.canonical_slug);
    ("name", `String m.name);
    ("created", `Int m.created);
    ("pricing", yojson_of_pricing m.pricing);
    ("architecture", yojson_of_architecture m.architecture);
    ("top_provider", yojson_of_top_provider m.top_provider);
    ("supported_parameters", `List (List.map (fun s -> `String s) m.supported_parameters));
  ] in
  let fields = match m.context_length with Some c -> ("context_length", `Int c) :: fields | None -> ("context_length", `Null) :: fields in
  let fields = match m.per_request_limits with Some p -> ("per_request_limits", yojson_of_per_request_limits p) :: fields | None -> ("per_request_limits", `Null) :: fields in
  let fields = match m.default_parameters with Some d -> ("default_parameters", yojson_of_default_parameters d) :: fields | None -> ("default_parameters", `Null) :: fields in
  let fields = match m.hugging_face_id with Some h -> ("hugging_face_id", `String h) :: fields | None -> fields in
  let fields = match m.description with Some d -> ("description", `String d) :: fields | None -> fields in
  let fields = match m.expiration_date with Some e -> ("expiration_date", `String e) :: fields | None -> ("expiration_date", `Null) :: fields in
  `Assoc fields

(** Response from listing models *)
type list_response = {
  data : t list;
}

let list_response_of_yojson json =
  let open Yojson.Safe.Util in
  { data = json |> member "data" |> to_list |> List.map t_of_yojson }

let yojson_of_list_response r =
  `Assoc [("data", `List (List.map yojson_of_t r.data))]

(** Response from counting models *)
type count_response_data = {
  count : int;
}

let count_response_data_of_yojson json =
  let open Yojson.Safe.Util in
  { count = json |> member "count" |> to_int }

let yojson_of_count_response_data d =
  `Assoc [("count", `Int d.count)]

type count_response = {
  data : count_response_data;
}

let count_response_of_yojson json =
  let open Yojson.Safe.Util in
  { data = json |> member "data" |> count_response_data_of_yojson }

let yojson_of_count_response r =
  `Assoc [("data", yojson_of_count_response_data r.data)]

(** Endpoint information for a model *)
type endpoint = {
  name : string;
  context_length : int option;
  pricing : pricing option;
  provider_name : string option;
}

let endpoint_of_yojson json =
  let open Yojson.Safe.Util in
  {
    name = json |> member "name" |> to_string;
    context_length = json |> member "context_length" |> to_int_option;
    pricing = (match json |> member "pricing" with
      | `Null -> None
      | j -> Some (pricing_of_yojson j));
    provider_name = json |> member "provider_name" |> to_string_option;
  }

let yojson_of_endpoint e =
  let fields = [("name", `String e.name)] in
  let fields = match e.context_length with Some c -> ("context_length", `Int c) :: fields | None -> fields in
  let fields = match e.pricing with Some p -> ("pricing", yojson_of_pricing p) :: fields | None -> fields in
  let fields = match e.provider_name with Some n -> ("provider_name", `String n) :: fields | None -> fields in
  `Assoc fields

type endpoints_response = {
  data : endpoint list;
}

let endpoints_response_of_yojson json =
  let open Yojson.Safe.Util in
  { data = json |> member "data" |> to_list |> List.map endpoint_of_yojson }

let yojson_of_endpoints_response r =
  `Assoc [("data", `List (List.map yojson_of_endpoint r.data))]
