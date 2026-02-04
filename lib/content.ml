(** Content types for messages (text, images, etc.) *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type image_detail = Auto | Low | High

let yojson_of_image_detail = function
  | Auto -> `String "auto"
  | Low -> `String "low"
  | High -> `String "high"

let image_detail_of_yojson = function
  | `String "auto" -> Auto
  | `String "low" -> Low
  | `String "high" -> High
  | json ->
      Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
        "Invalid image detail: expected auto, low, or high" json

type image_url = {
  url : string;
  detail : image_detail option; [@yojson.option]
}
[@@deriving yojson]

type text_part = {
  type_ : string; [@key "type"]
  text : string;
}
[@@deriving yojson]

type image_part = {
  type_ : string; [@key "type"]
  image_url : image_url;
}
[@@deriving yojson]

(** Audio input data *)
type input_audio = {
  data : string;  (* Base64 encoded audio *)
  format : string; (* e.g., "wav", "mp3" *)
}
[@@deriving yojson]

type audio_part = {
  type_ : string; [@key "type"]
  input_audio : input_audio;
}
[@@deriving yojson]

(** Video URL *)
type video_url = {
  url : string;
}
[@@deriving yojson]

type video_part = {
  type_ : string; [@key "type"]
  video_url : video_url;
}
[@@deriving yojson]

type content_part =
  | Text_part of text_part
  | Image_part of image_part
  | Audio_part of audio_part
  | Video_part of video_part

let yojson_of_content_part = function
  | Text_part t -> yojson_of_text_part t
  | Image_part i -> yojson_of_image_part i
  | Audio_part a -> yojson_of_audio_part a
  | Video_part v -> yojson_of_video_part v

let content_part_of_yojson json =
  let open Yojson.Safe.Util in
  let type_ = json |> member "type" |> to_string in
  match type_ with
  | "text" -> Text_part (text_part_of_yojson json)
  | "image_url" -> Image_part (image_part_of_yojson json)
  | "input_audio" -> Audio_part (audio_part_of_yojson json)
  | "video_url" -> Video_part (video_part_of_yojson json)
  | _ ->
      Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
        ("Unknown content part type: " ^ type_) json

type content = String_content of string | Parts_content of content_part list

let yojson_of_content = function
  | String_content s -> `String s
  | Parts_content parts -> `List (List.map yojson_of_content_part parts)

let content_of_yojson = function
  | `String s -> String_content s
  | `List parts -> Parts_content (List.map content_part_of_yojson parts)
  | json ->
      Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
        "Invalid content: expected string or array" json

(** Convenience constructors *)

let text s = String_content s

let text_part s = Text_part { type_ = "text"; text = s }

let image_url ?detail url =
  Image_part { type_ = "image_url"; image_url = { url; detail } }

let image_base64 ?detail ~media_type data =
  let url = Printf.sprintf "data:%s;base64,%s" media_type (Base64.encode_string data) in
  image_url ?detail url

(** Create an audio content part from base64 data *)
let audio_base64 ~format data =
  Audio_part { type_ = "input_audio"; input_audio = { data; format } }

(** Create an audio content part from raw bytes *)
let audio_bytes ~format bytes =
  let data = Base64.encode_string bytes in
  audio_base64 ~format data

(** Create a video content part from URL *)
let video_url url =
  Video_part { type_ = "video_url"; video_url = { url } }

(** Create a video content part from base64 data *)
let video_base64 ~media_type data =
  let url = Printf.sprintf "data:%s;base64,%s" media_type (Base64.encode_string data) in
  video_url url

let parts ps = Parts_content ps
