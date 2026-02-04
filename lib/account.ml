(** Account-related types and responses *)

(* Helper to handle JSON numbers that can be int or float *)
let to_number_option json =
  match json with
  | `Float f -> Some f
  | `Int i -> Some (float_of_int i)
  | `Null -> None
  | _ -> None

let to_number json =
  match json with
  | `Float f -> f
  | `Int i -> float_of_int i
  | _ -> failwith "Expected number"

(** Credits response *)
type credits_data = {
  total_credits : float;
  total_usage : float;
}

let credits_data_of_yojson json =
  let open Yojson.Safe.Util in
  {
    total_credits = json |> member "total_credits" |> to_number;
    total_usage = json |> member "total_usage" |> to_number;
  }

let yojson_of_credits_data d =
  `Assoc [
    ("total_credits", `Float d.total_credits);
    ("total_usage", `Float d.total_usage);
  ]

type credits_response = {
  data : credits_data;
}

let credits_response_of_yojson json =
  let open Yojson.Safe.Util in
  { data = json |> member "data" |> credits_data_of_yojson }

let yojson_of_credits_response r =
  `Assoc [("data", yojson_of_credits_data r.data)]

(** Generation metadata response *)
type generation_data = {
  id : string;
  model : string option;
  object_ : string option;
  created : int option;
  provider : string option;
  total_cost : float option;
  generation_time : float option;
  tokens_prompt : int option;
  tokens_completion : int option;
  native_tokens_prompt : int option;
  native_tokens_completion : int option;
  native_tokens_reasoning : int option;
  num_media_prompt : int option;
  num_media_completion : int option;
  origin : string option;
  streamed : bool option;
  cancelled : bool option;
  finish_reason : string option;
  moderation_latency : float option;
  upstream_id : string option;
}

let generation_data_of_yojson json =
  let open Yojson.Safe.Util in
  {
    id = json |> member "id" |> to_string;
    model = json |> member "model" |> to_string_option;
    object_ = json |> member "object" |> to_string_option;
    created = json |> member "created" |> to_int_option;
    provider = json |> member "provider" |> to_string_option;
    total_cost = json |> member "total_cost" |> to_number_option;
    generation_time = json |> member "generation_time" |> to_number_option;
    tokens_prompt = json |> member "tokens_prompt" |> to_int_option;
    tokens_completion = json |> member "tokens_completion" |> to_int_option;
    native_tokens_prompt = json |> member "native_tokens_prompt" |> to_int_option;
    native_tokens_completion = json |> member "native_tokens_completion" |> to_int_option;
    native_tokens_reasoning = json |> member "native_tokens_reasoning" |> to_int_option;
    num_media_prompt = json |> member "num_media_prompt" |> to_int_option;
    num_media_completion = json |> member "num_media_completion" |> to_int_option;
    origin = json |> member "origin" |> to_string_option;
    streamed = json |> member "streamed" |> to_bool_option;
    cancelled = json |> member "cancelled" |> to_bool_option;
    finish_reason = json |> member "finish_reason" |> to_string_option;
    moderation_latency = json |> member "moderation_latency" |> to_number_option;
    upstream_id = json |> member "upstream_id" |> to_string_option;
  }

let yojson_of_generation_data g =
  let fields = [("id", `String g.id)] in
  let add_str name v fields = match v with Some s -> (name, `String s) :: fields | None -> fields in
  let add_int name v fields = match v with Some i -> (name, `Int i) :: fields | None -> fields in
  let add_float name v fields = match v with Some f -> (name, `Float f) :: fields | None -> fields in
  let add_bool name v fields = match v with Some b -> (name, `Bool b) :: fields | None -> fields in
  let fields = add_str "model" g.model fields in
  let fields = add_str "object" g.object_ fields in
  let fields = add_int "created" g.created fields in
  let fields = add_str "provider" g.provider fields in
  let fields = add_float "total_cost" g.total_cost fields in
  let fields = add_float "generation_time" g.generation_time fields in
  let fields = add_int "tokens_prompt" g.tokens_prompt fields in
  let fields = add_int "tokens_completion" g.tokens_completion fields in
  let fields = add_int "native_tokens_prompt" g.native_tokens_prompt fields in
  let fields = add_int "native_tokens_completion" g.native_tokens_completion fields in
  let fields = add_int "native_tokens_reasoning" g.native_tokens_reasoning fields in
  let fields = add_int "num_media_prompt" g.num_media_prompt fields in
  let fields = add_int "num_media_completion" g.num_media_completion fields in
  let fields = add_str "origin" g.origin fields in
  let fields = add_bool "streamed" g.streamed fields in
  let fields = add_bool "cancelled" g.cancelled fields in
  let fields = add_str "finish_reason" g.finish_reason fields in
  let fields = add_float "moderation_latency" g.moderation_latency fields in
  let fields = add_str "upstream_id" g.upstream_id fields in
  `Assoc fields

type generation_response = {
  data : generation_data;
}

let generation_response_of_yojson json =
  let open Yojson.Safe.Util in
  { data = json |> member "data" |> generation_data_of_yojson }

let yojson_of_generation_response r =
  `Assoc [("data", yojson_of_generation_data r.data)]

(** Remaining credits *)
let remaining credits = credits.total_credits -. credits.total_usage
