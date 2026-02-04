(** Tool and function call types *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Yojson.Safe.t converters for opaque fields *)
let yojson_of_yojson_safe_t (x : Yojson.Safe.t) : Yojson.Safe.t = x
let yojson_safe_t_of_yojson (x : Yojson.Safe.t) : Yojson.Safe.t = x

type function_def = {
  name : string;
  description : string option; [@yojson.option]
  parameters : Yojson.Safe.t option; [@yojson.option]
  strict : bool option; [@yojson.option]
}

let yojson_of_function_def f =
  let fields = [("name", `String f.name)] in
  let fields =
    match f.description with
    | Some d -> ("description", `String d) :: fields
    | None -> fields
  in
  let fields =
    match f.parameters with
    | Some p -> ("parameters", p) :: fields
    | None -> fields
  in
  let fields =
    match f.strict with
    | Some s -> ("strict", `Bool s) :: fields
    | None -> fields
  in
  `Assoc (List.rev fields)

let function_def_of_yojson json =
  let open Yojson.Safe.Util in
  {
    name = json |> member "name" |> to_string;
    description = json |> member "description" |> to_string_option;
    parameters =
      (match json |> member "parameters" with
       | `Null -> None
       | x -> Some x);
    strict = json |> member "strict" |> to_bool_option;
  }

type tool = {
  type_ : string; [@key "type"]
  function_ : function_def; [@key "function"]
}

let yojson_of_tool t =
  `Assoc [("type", `String t.type_); ("function", yojson_of_function_def t.function_)]

let tool_of_yojson json =
  let open Yojson.Safe.Util in
  {
    type_ = json |> member "type" |> to_string;
    function_ = json |> member "function" |> function_def_of_yojson;
  }

type function_call = {
  name : string;
  arguments : string;
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

type tool_call = {
  id : string;
  type_ : string; [@key "type"]
  function_ : function_call; [@key "function"]
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

type function_call_delta = {
  name : string option; [@yojson.option]
  arguments : string option; [@yojson.option]
}
[@@deriving yojson]

type tool_call_delta = {
  index : int;
  id : string option; [@yojson.option]
  type_ : string option; [@yojson.option] [@key "type"]
  function_ : function_call_delta option; [@yojson.option] [@key "function"]
}
[@@deriving yojson]

type tool_choice =
  | Auto
  | None_choice
  | Required
  | Function_choice of string

let yojson_of_tool_choice = function
  | Auto -> `String "auto"
  | None_choice -> `String "none"
  | Required -> `String "required"
  | Function_choice name ->
      `Assoc
        [ ("type", `String "function"); ("function", `Assoc [ ("name", `String name) ]) ]

let tool_choice_of_yojson = function
  | `String "auto" -> Auto
  | `String "none" -> None_choice
  | `String "required" -> Required
  | `Assoc _ as json ->
      let open Yojson.Safe.Util in
      let func = json |> member "function" in
      let name = func |> member "name" |> to_string in
      Function_choice name
  | json ->
      Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
        "Invalid tool_choice format" json

(** Convenience function to create a tool *)
let make_function ?description ?parameters ?strict name =
  {
    type_ = "function";
    function_ = { name; description; parameters; strict };
  }
