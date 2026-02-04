(** Embeddings API types *)

(* Helper to handle JSON numbers that can be int or float *)
let to_number json =
  match json with
  | `Float f -> f
  | `Int i -> float_of_int i
  | _ -> failwith "Expected number"

(** Encoding format for embeddings *)
type encoding_format =
  | Float
  | Base64

let yojson_of_encoding_format = function
  | Float -> `String "float"
  | Base64 -> `String "base64"

let encoding_format_of_yojson = function
  | `String "float" -> Float
  | `String "base64" -> Base64
  | _ -> Float (* default *)

(** Input type hint *)
type input_type =
  | Search_query
  | Search_document
  | Classification
  | Clustering

let yojson_of_input_type = function
  | Search_query -> `String "search_query"
  | Search_document -> `String "search_document"
  | Classification -> `String "classification"
  | Clustering -> `String "clustering"

(** Embedding input - can be string or list of strings *)
type input =
  | Single of string
  | Multiple of string list

let yojson_of_input = function
  | Single s -> `String s
  | Multiple l -> `List (List.map (fun s -> `String s) l)

(** Single embedding data *)
type embedding_data = {
  object_ : string;
  embedding : float list;
  index : int;
}

let embedding_data_of_yojson json =
  let open Yojson.Safe.Util in
  {
    object_ = json |> member "object" |> to_string;
    embedding = json |> member "embedding" |> to_list |> List.map to_number;
    index = json |> member "index" |> to_int;
  }

let yojson_of_embedding_data e =
  `Assoc [
    ("object", `String e.object_);
    ("embedding", `List (List.map (fun f -> `Float f) e.embedding));
    ("index", `Int e.index);
  ]

(** Usage information *)
type usage = {
  prompt_tokens : int;
  total_tokens : int;
  cost : float option;
}

let usage_of_yojson json =
  let open Yojson.Safe.Util in
  {
    prompt_tokens = json |> member "prompt_tokens" |> to_int;
    total_tokens = json |> member "total_tokens" |> to_int;
    cost = (match json |> member "cost" with
      | `Null -> None
      | `Float f -> Some f
      | `Int i -> Some (float_of_int i)
      | _ -> None);
  }

let yojson_of_usage u =
  let fields = [
    ("prompt_tokens", `Int u.prompt_tokens);
    ("total_tokens", `Int u.total_tokens);
  ] in
  let fields = match u.cost with
    | Some c -> ("cost", `Float c) :: fields
    | None -> fields
  in
  `Assoc fields

(** Embeddings response *)
type response = {
  object_ : string;
  data : embedding_data list;
  model : string;
  usage : usage;
  provider : string option;
  id : string option;
}

let response_of_yojson json =
  let open Yojson.Safe.Util in
  {
    object_ = json |> member "object" |> to_string;
    data = json |> member "data" |> to_list |> List.map embedding_data_of_yojson;
    model = json |> member "model" |> to_string;
    usage = json |> member "usage" |> usage_of_yojson;
    provider = json |> member "provider" |> to_string_option;
    id = json |> member "id" |> to_string_option;
  }

let yojson_of_response r =
  let fields = [
    ("object", `String r.object_);
    ("data", `List (List.map yojson_of_embedding_data r.data));
    ("model", `String r.model);
    ("usage", yojson_of_usage r.usage);
  ] in
  let fields = match r.provider with
    | Some p -> ("provider", `String p) :: fields
    | None -> fields
  in
  let fields = match r.id with
    | Some id -> ("id", `String id) :: fields
    | None -> fields
  in
  `Assoc fields

(** Embeddings request *)
type request = {
  input : input;
  model : string;
  encoding_format : encoding_format option;
  dimensions : int option;
  user : string option;
  input_type : input_type option;
}

let make_request ?encoding_format ?dimensions ?user ?input_type ~model input =
  { input; model; encoding_format; dimensions; user; input_type }

let yojson_of_request r =
  let fields = [
    ("input", yojson_of_input r.input);
    ("model", `String r.model);
  ] in
  let fields = match r.encoding_format with
    | Some ef -> ("encoding_format", yojson_of_encoding_format ef) :: fields
    | None -> fields
  in
  let fields = match r.dimensions with
    | Some d -> ("dimensions", `Int d) :: fields
    | None -> fields
  in
  let fields = match r.user with
    | Some u -> ("user", `String u) :: fields
    | None -> fields
  in
  let fields = match r.input_type with
    | Some it -> ("input_type", yojson_of_input_type it) :: fields
    | None -> fields
  in
  `Assoc fields

(** Get the first embedding vector *)
let get_embedding response =
  match response.data with
  | [] -> None
  | e :: _ -> Some e.embedding

(** Get all embedding vectors *)
let get_embeddings response =
  List.map (fun e -> e.embedding) response.data

(** Calculate cosine similarity between two embeddings *)
let cosine_similarity a b =
  let dot = List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0.0 a b in
  let norm_a = sqrt (List.fold_left (fun acc x -> acc +. (x *. x)) 0.0 a) in
  let norm_b = sqrt (List.fold_left (fun acc x -> acc +. (x *. x)) 0.0 b) in
  if norm_a = 0.0 || norm_b = 0.0 then 0.0
  else dot /. (norm_a *. norm_b)
