(** Text completions types (non-chat API) *)

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

(** Completion choice logprobs *)
type logprobs = {
  tokens : string list option;
  token_logprobs : float list option;
  top_logprobs : Yojson.Safe.t list option;
  text_offset : int list option;
}

let logprobs_of_yojson json =
  let open Yojson.Safe.Util in
  {
    tokens = (match json |> member "tokens" with
      | `Null -> None
      | `List l -> Some (List.map to_string l)
      | _ -> None);
    token_logprobs = (match json |> member "token_logprobs" with
      | `Null -> None
      | `List l -> Some (List.map to_number l)
      | _ -> None);
    top_logprobs = (match json |> member "top_logprobs" with
      | `Null -> None
      | `List l -> Some l
      | _ -> None);
    text_offset = (match json |> member "text_offset" with
      | `Null -> None
      | `List l -> Some (List.map to_int l)
      | _ -> None);
  }

let yojson_of_logprobs lp =
  let fields = [] in
  let add_list name conv v fields = match v with
    | Some l -> (name, `List (List.map conv l)) :: fields
    | None -> (name, `Null) :: fields
  in
  let fields = add_list "tokens" (fun s -> `String s) lp.tokens fields in
  let fields = add_list "token_logprobs" (fun f -> `Float f) lp.token_logprobs fields in
  let fields = add_list "top_logprobs" (fun x -> x) lp.top_logprobs fields in
  let fields = add_list "text_offset" (fun i -> `Int i) lp.text_offset fields in
  `Assoc fields

(** Completion choice *)
type choice = {
  text : string;
  index : int;
  logprobs : logprobs option;
  finish_reason : string option;
  native_finish_reason : string option;
  reasoning : string option;
}

let choice_of_yojson json =
  let open Yojson.Safe.Util in
  {
    text = json |> member "text" |> to_string;
    index = json |> member "index" |> to_int;
    logprobs = (match json |> member "logprobs" with
      | `Null -> None
      | j -> Some (logprobs_of_yojson j));
    finish_reason = json |> member "finish_reason" |> to_string_option;
    native_finish_reason = json |> member "native_finish_reason" |> to_string_option;
    reasoning = json |> member "reasoning" |> to_string_option;
  }

let yojson_of_choice c =
  let fields = [
    ("text", `String c.text);
    ("index", `Int c.index);
  ] in
  let fields = match c.logprobs with
    | Some lp -> ("logprobs", yojson_of_logprobs lp) :: fields
    | None -> ("logprobs", `Null) :: fields
  in
  let fields = match c.finish_reason with
    | Some fr -> ("finish_reason", `String fr) :: fields
    | None -> ("finish_reason", `Null) :: fields
  in
  let fields = match c.native_finish_reason with
    | Some nfr -> ("native_finish_reason", `String nfr) :: fields
    | None -> fields
  in
  let fields = match c.reasoning with
    | Some r -> ("reasoning", `String r) :: fields
    | None -> fields
  in
  `Assoc fields

(** Completion usage *)
type usage = {
  prompt_tokens : int;
  completion_tokens : int;
  total_tokens : int;
}

let usage_of_yojson json =
  let open Yojson.Safe.Util in
  {
    prompt_tokens = json |> member "prompt_tokens" |> to_int;
    completion_tokens = json |> member "completion_tokens" |> to_int;
    total_tokens = json |> member "total_tokens" |> to_int;
  }

let yojson_of_usage u =
  `Assoc [
    ("prompt_tokens", `Int u.prompt_tokens);
    ("completion_tokens", `Int u.completion_tokens);
    ("total_tokens", `Int u.total_tokens);
  ]

(** Completion response *)
type response = {
  id : string;
  object_ : string;
  created : int;
  model : string;
  choices : choice list;
  provider : string option;
  system_fingerprint : string option;
  usage : usage option;
}

let response_of_yojson json =
  let open Yojson.Safe.Util in
  {
    id = json |> member "id" |> to_string;
    object_ = json |> member "object" |> to_string;
    created = json |> member "created" |> to_int;
    model = json |> member "model" |> to_string;
    choices = json |> member "choices" |> to_list |> List.map choice_of_yojson;
    provider = json |> member "provider" |> to_string_option;
    system_fingerprint = json |> member "system_fingerprint" |> to_string_option;
    usage = (match json |> member "usage" with
      | `Null -> None
      | j -> Some (usage_of_yojson j));
  }

let yojson_of_response r =
  let fields = [
    ("id", `String r.id);
    ("object", `String r.object_);
    ("created", `Int r.created);
    ("model", `String r.model);
    ("choices", `List (List.map yojson_of_choice r.choices));
  ] in
  let fields = match r.provider with
    | Some p -> ("provider", `String p) :: fields
    | None -> fields
  in
  let fields = match r.system_fingerprint with
    | Some sf -> ("system_fingerprint", `String sf) :: fields
    | None -> fields
  in
  let fields = match r.usage with
    | Some u -> ("usage", yojson_of_usage u) :: fields
    | None -> fields
  in
  `Assoc fields

(** Prompt type - can be string or list of strings *)
type prompt =
  | String of string
  | Strings of string list

let yojson_of_prompt = function
  | String s -> `String s
  | Strings l -> `List (List.map (fun s -> `String s) l)

(** Completion request *)
type request = {
  prompt : prompt;
  model : string option;
  models : string list option;
  best_of : int option;
  echo : bool option;
  frequency_penalty : float option;
  logprobs : int option;
  max_tokens : int option;
  n : int option;
  presence_penalty : float option;
  seed : int option;
  stop : string list option;
  stream : bool;
  suffix : string option;
  temperature : float option;
  top_p : float option;
  user : string option;
}

let make_request ?model ?models ?best_of ?echo ?frequency_penalty ?logprobs
    ?max_tokens ?n ?presence_penalty ?seed ?stop ?(stream = false) ?suffix
    ?temperature ?top_p ?user prompt =
  {
    prompt;
    model;
    models;
    best_of;
    echo;
    frequency_penalty;
    logprobs;
    max_tokens;
    n;
    presence_penalty;
    seed;
    stop;
    stream;
    suffix;
    temperature;
    top_p;
    user;
  }

let yojson_of_request r =
  let fields = [("prompt", yojson_of_prompt r.prompt)] in
  let add_str name v fields = match v with Some s -> (name, `String s) :: fields | None -> fields in
  let add_int name v fields = match v with Some i -> (name, `Int i) :: fields | None -> fields in
  let add_float name v fields = match v with Some f -> (name, `Float f) :: fields | None -> fields in
  let add_bool name v fields = match v with Some b -> (name, `Bool b) :: fields | None -> fields in
  let fields = add_str "model" r.model fields in
  let fields = match r.models with
    | Some ms -> ("models", `List (List.map (fun s -> `String s) ms)) :: fields
    | None -> fields
  in
  let fields = add_int "best_of" r.best_of fields in
  let fields = add_bool "echo" r.echo fields in
  let fields = add_float "frequency_penalty" r.frequency_penalty fields in
  let fields = add_int "logprobs" r.logprobs fields in
  let fields = add_int "max_tokens" r.max_tokens fields in
  let fields = add_int "n" r.n fields in
  let fields = add_float "presence_penalty" r.presence_penalty fields in
  let fields = add_int "seed" r.seed fields in
  let fields = match r.stop with
    | Some stops -> ("stop", `List (List.map (fun s -> `String s) stops)) :: fields
    | None -> fields
  in
  let fields = ("stream", `Bool r.stream) :: fields in
  let fields = add_str "suffix" r.suffix fields in
  let fields = add_float "temperature" r.temperature fields in
  let fields = add_float "top_p" r.top_p fields in
  let fields = add_str "user" r.user fields in
  `Assoc fields

(** Get text from first choice *)
let get_text response =
  match response.choices with
  | [] -> None
  | choice :: _ -> Some choice.text
