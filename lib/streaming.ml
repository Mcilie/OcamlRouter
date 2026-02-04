(** Streaming response types *)

(* Custom delta type to handle all the fields flexibly *)
type delta = {
  role : string option;
  content : string option;
  tool_calls : Tool.tool_call_delta list option;
  reasoning : string option;
}

let yojson_of_delta d =
  let fields = [] in
  let fields = match d.role with
    | Some r -> ("role", `String r) :: fields
    | None -> fields in
  let fields = match d.content with
    | Some c -> ("content", `String c) :: fields
    | None -> fields in
  let fields = match d.tool_calls with
    | Some tcs -> ("tool_calls", `List (List.map Tool.yojson_of_tool_call_delta tcs)) :: fields
    | None -> fields in
  let fields = match d.reasoning with
    | Some r -> ("reasoning", `String r) :: fields
    | None -> fields in
  `Assoc (List.rev fields)

let delta_of_yojson json =
  let open Yojson.Safe.Util in
  let role = json |> member "role" |> to_string_option in
  let content = json |> member "content" |> to_string_option in
  let tool_calls = match json |> member "tool_calls" with
    | `Null -> None
    | `List l -> Some (List.map Tool.tool_call_delta_of_yojson l)
    | _ -> None in
  let reasoning = json |> member "reasoning" |> to_string_option in
  { role; content; tool_calls; reasoning }

type choice = {
  index : int;
  delta : delta;
  finish_reason : string option;
}

let yojson_of_choice c =
  let fields = [
    ("index", `Int c.index);
    ("delta", yojson_of_delta c.delta);
  ] in
  let fields = match c.finish_reason with
    | Some r -> ("finish_reason", `String r) :: fields
    | None -> fields in
  `Assoc (List.rev fields)

let choice_of_yojson json =
  let open Yojson.Safe.Util in
  let index = json |> member "index" |> to_int in
  let delta = json |> member "delta" |> delta_of_yojson in
  let finish_reason = json |> member "finish_reason" |> to_string_option in
  { index; delta; finish_reason }

type chunk = {
  id : string;
  model : string;
  created : int;
  choices : choice list;
  usage : Common.usage option;
}

let yojson_of_chunk c =
  let fields = [
    ("id", `String c.id);
    ("model", `String c.model);
    ("created", `Int c.created);
    ("choices", `List (List.map yojson_of_choice c.choices));
  ] in
  let fields = match c.usage with
    | Some u -> ("usage", Common.yojson_of_usage u) :: fields
    | None -> fields in
  `Assoc (List.rev fields)

let chunk_of_yojson json =
  let open Yojson.Safe.Util in
  let id = json |> member "id" |> to_string in
  let model = json |> member "model" |> to_string in
  let created = json |> member "created" |> to_int in
  let choices = json |> member "choices" |> to_list |> List.map choice_of_yojson in
  let usage = match json |> member "usage" with
    | `Null -> None
    | u -> Some (Common.usage_of_yojson u) in
  { id; model; created; choices; usage }

(** Extract text content from a chunk *)
let get_content chunk =
  match chunk.choices with
  | [] -> None
  | choice :: _ -> choice.delta.content

(** Extract reasoning content from a chunk *)
let get_reasoning chunk =
  match chunk.choices with
  | [] -> None
  | choice :: _ -> choice.delta.reasoning

(** Check if stream is finished *)
let is_finished chunk =
  match chunk.choices with
  | [] -> false
  | choice :: _ -> Option.is_some choice.finish_reason
