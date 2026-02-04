(** Message types for chat conversations *)

type role = System | User | Assistant | Tool

let yojson_of_role = function
  | System -> `String "system"
  | User -> `String "user"
  | Assistant -> `String "assistant"
  | Tool -> `String "tool"

let role_of_yojson = function
  | `String "system" -> System
  | `String "user" -> User
  | `String "assistant" -> Assistant
  | `String "tool" -> Tool
  | json ->
      Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
        "Invalid role: expected system, user, assistant, or tool" json

type t = {
  role : role;
  content : Content.content option;
  name : string option;
  tool_call_id : string option;
  tool_calls : Tool.tool_call list option;
  refusal : string option;
  reasoning : string option;
}

(* Custom JSON handling to deal with null values *)
let yojson_of_t t =
  let fields = [("role", yojson_of_role t.role)] in
  let fields = match t.content with
    | Some c -> ("content", Content.yojson_of_content c) :: fields
    | None -> fields in
  let fields = match t.name with
    | Some n -> ("name", `String n) :: fields
    | None -> fields in
  let fields = match t.tool_call_id with
    | Some id -> ("tool_call_id", `String id) :: fields
    | None -> fields in
  let fields = match t.tool_calls with
    | Some tcs -> ("tool_calls", `List (List.map Tool.yojson_of_tool_call tcs)) :: fields
    | None -> fields in
  let fields = match t.refusal with
    | Some r -> ("refusal", `String r) :: fields
    | None -> fields in
  let fields = match t.reasoning with
    | Some r -> ("reasoning", `String r) :: fields
    | None -> fields in
  `Assoc (List.rev fields)

let t_of_yojson json =
  let open Yojson.Safe.Util in
  let role = json |> member "role" |> role_of_yojson in
  let content = match json |> member "content" with
    | `Null -> None
    | c -> Some (Content.content_of_yojson c) in
  let name = json |> member "name" |> to_string_option in
  let tool_call_id = json |> member "tool_call_id" |> to_string_option in
  let tool_calls = match json |> member "tool_calls" with
    | `Null -> None
    | `List l -> Some (List.map Tool.tool_call_of_yojson l)
    | _ -> None in
  let refusal = json |> member "refusal" |> to_string_option in
  let reasoning = json |> member "reasoning" |> to_string_option in
  { role; content; name; tool_call_id; tool_calls; refusal; reasoning }

(** Convenience constructors *)

let system content =
  { role = System; content = Some (Content.text content); name = None; tool_call_id = None; tool_calls = None; refusal = None; reasoning = None }

let user content =
  { role = User; content = Some (Content.text content); name = None; tool_call_id = None; tool_calls = None; refusal = None; reasoning = None }

let user_with_parts parts =
  { role = User; content = Some (Content.parts parts); name = None; tool_call_id = None; tool_calls = None; refusal = None; reasoning = None }

let assistant content =
  { role = Assistant; content = Some (Content.text content); name = None; tool_call_id = None; tool_calls = None; refusal = None; reasoning = None }

let assistant_with_tool_calls ?content tool_calls =
  {
    role = Assistant;
    content = Option.map Content.text content;
    name = None;
    tool_call_id = None;
    tool_calls = Some tool_calls;
    refusal = None;
    reasoning = None;
  }

let tool ~tool_call_id content =
  {
    role = Tool;
    content = Some (Content.text content);
    name = None;
    tool_call_id = Some tool_call_id;
    tool_calls = None;
    refusal = None;
    reasoning = None;
  }
