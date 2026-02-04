(** Reasoning configuration for models that support extended thinking *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type effort = Low | Medium | High

let yojson_of_effort = function
  | Low -> `String "low"
  | Medium -> `String "medium"
  | High -> `String "high"

let effort_of_yojson = function
  | `String "low" -> Low
  | `String "medium" -> Medium
  | `String "high" -> High
  | json ->
      Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
        "Invalid reasoning effort: expected low, medium, or high" json

type t = {
  effort : effort option; [@yojson.option]
  max_tokens : int option; [@yojson.option]
}
[@@deriving yojson]

let make ?effort ?max_tokens () = { effort; max_tokens }
