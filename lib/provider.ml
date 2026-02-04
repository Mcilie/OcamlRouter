(** Provider preferences for routing *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type order = string list [@@deriving yojson]

type quantization =
  | Int4
  | Int8
  | Fp6
  | Fp8
  | Fp16
  | Bf16
  | Unknown of string

let yojson_of_quantization = function
  | Int4 -> `String "int4"
  | Int8 -> `String "int8"
  | Fp6 -> `String "fp6"
  | Fp8 -> `String "fp8"
  | Fp16 -> `String "fp16"
  | Bf16 -> `String "bf16"
  | Unknown s -> `String s

let quantization_of_yojson = function
  | `String "int4" -> Int4
  | `String "int8" -> Int8
  | `String "fp6" -> Fp6
  | `String "fp8" -> Fp8
  | `String "fp16" -> Fp16
  | `String "bf16" -> Bf16
  | `String s -> Unknown s
  | json ->
      Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
        "Invalid quantization value" json

type preferences = {
  allow_fallbacks : bool option; [@yojson.option]
  require_parameters : bool option; [@yojson.option]
  data_collection : string option; [@yojson.option]
  order : order option; [@yojson.option]
  ignore_ : string list option; [@yojson.option] [@key "ignore"]
  quantizations : quantization list option; [@yojson.option]
}
[@@deriving yojson]

type route = Fallback

let yojson_of_route = function Fallback -> `String "fallback"

let route_of_yojson = function
  | `String "fallback" -> Fallback
  | json ->
      Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
        "Invalid route: expected fallback" json

let make_preferences ?allow_fallbacks ?require_parameters ?data_collection ?order
    ?ignore_ ?quantizations () =
  { allow_fallbacks; require_parameters; data_collection; order; ignore_; quantizations }
