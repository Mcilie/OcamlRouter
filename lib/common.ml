(** Common types used across the SDK *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type usage = {
  prompt_tokens : int;
  completion_tokens : int;
  total_tokens : int;
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

type json_schema_format = {
  name : string;
  schema : Yojson.Safe.t;
  strict : bool option;
}

type response_format =
  | Text_format
  | Json_object_format
  | Json_schema_format of json_schema_format

let yojson_of_response_format = function
  | Text_format -> `Assoc [ ("type", `String "text") ]
  | Json_object_format -> `Assoc [ ("type", `String "json_object") ]
  | Json_schema_format schema ->
      let schema_fields =
        [("name", `String schema.name); ("schema", schema.schema)]
        @
        (match schema.strict with
         | Some s -> [("strict", `Bool s)]
         | None -> [])
      in
      `Assoc [("type", `String "json_schema"); ("json_schema", `Assoc schema_fields)]

let response_format_of_yojson json =
  let open Yojson.Safe.Util in
  let type_ = json |> member "type" |> to_string in
  match type_ with
  | "text" -> Text_format
  | "json_object" -> Json_object_format
  | "json_schema" ->
      let js = json |> member "json_schema" in
      let name = js |> member "name" |> to_string in
      let schema = js |> member "schema" in
      let strict = js |> member "strict" |> to_bool_option in
      Json_schema_format { name; schema; strict }
  | _ ->
      Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
        ("Unknown response_format type: " ^ type_) json
