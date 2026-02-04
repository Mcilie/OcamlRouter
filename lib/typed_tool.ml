(** Typed Tool System - Type-safe layer on top of Tool

    This module provides a type-safe way to define tools and handle tool calls
    with pattern matching. It builds on top of the existing Tool module.

    {1 Example}

    {[
      (* Define a typed tool *)
      let weather_tool =
        Typed_tool.create
          ~name:"get_weather"
          ~description:"Get current weather for a location"
          ~schema:Schema.(obj ~required:["location"] [
            "location", string;
            "unit", enum ["celsius"; "fahrenheit"];
          ])
          ~parse:(fun json ->
            let open Yojson.Safe.Util in
            (json |> member "location" |> to_string,
             json |> member "unit" |> to_string_option))
          ()

      (* Use in a request *)
      let tools = [Typed_tool.to_tool weather_tool]

      (* Handle tool calls with pattern matching *)
      match tool_calls with
      | [call] when Typed_tool.matches weather_tool call ->
        let (location, unit) = Typed_tool.parse_call_exn weather_tool call in
        handle_weather location unit
      | _ -> ...
    ]}
*)

(** {1 Schema Builders}

    Helpers to build JSON schemas without writing raw JSON. *)
module Schema = struct
  (** String type *)
  let string = `Assoc [("type", `String "string")]

  (** String with description *)
  let string_desc desc =
    `Assoc [("type", `String "string"); ("description", `String desc)]

  (** Integer type *)
  let int = `Assoc [("type", `String "integer")]

  (** Integer with description *)
  let int_desc desc =
    `Assoc [("type", `String "integer"); ("description", `String desc)]

  (** Number (float) type *)
  let number = `Assoc [("type", `String "number")]

  (** Number with description *)
  let number_desc desc =
    `Assoc [("type", `String "number"); ("description", `String desc)]

  (** Boolean type *)
  let boolean = `Assoc [("type", `String "boolean")]

  (** Boolean with description *)
  let boolean_desc desc =
    `Assoc [("type", `String "boolean"); ("description", `String desc)]

  (** Null type *)
  let null = `Assoc [("type", `String "null")]

  (** Enum of string values *)
  let enum values =
    `Assoc [
      ("type", `String "string");
      ("enum", `List (List.map (fun s -> `String s) values));
    ]

  (** Enum with description *)
  let enum_desc desc values =
    `Assoc [
      ("type", `String "string");
      ("enum", `List (List.map (fun s -> `String s) values));
      ("description", `String desc);
    ]

  (** Array of items *)
  let array items =
    `Assoc [("type", `String "array"); ("items", items)]

  (** Array with description *)
  let array_desc desc items =
    `Assoc [
      ("type", `String "array");
      ("items", items);
      ("description", `String desc);
    ]

  (** Object with properties *)
  let obj ?(additional_properties = false) ~required properties =
    `Assoc [
      ("type", `String "object");
      ("properties", `Assoc properties);
      ("required", `List (List.map (fun s -> `String s) required));
      ("additionalProperties", `Bool additional_properties);
    ]

  (** Object with description *)
  let obj_desc ?(additional_properties = false) ~required desc properties =
    `Assoc [
      ("type", `String "object");
      ("properties", `Assoc properties);
      ("required", `List (List.map (fun s -> `String s) required));
      ("additionalProperties", `Bool additional_properties);
      ("description", `String desc);
    ]

  (** Optional wrapper - allows null *)
  let optional schema =
    match schema with
    | `Assoc fields ->
      `Assoc (fields @ [("nullable", `Bool true)])
    | _ -> schema

  (** Add a description to any schema *)
  let with_desc desc = function
    | `Assoc fields -> `Assoc (("description", `String desc) :: fields)
    | other -> other

  (** Combine schemas with anyOf *)
  let any_of schemas = `Assoc [("anyOf", `List schemas)]

  (** Combine schemas with oneOf *)
  let one_of schemas = `Assoc [("oneOf", `List schemas)]
end

(** {1 Typed Tool Definition} *)

(** A typed tool with parameter type ['a] *)
type 'a t = {
  name : string;
  description : string option;
  schema : Yojson.Safe.t;
  parse : Yojson.Safe.t -> 'a;
  strict : bool;
}

(** Create a new typed tool.

    @param name The function name
    @param description Optional description for the LLM
    @param schema JSON schema for parameters (use Schema module helpers)
    @param parse Function to parse JSON arguments into your type
    @param strict Whether to enforce strict schema validation (default: true)
*)
let create ~name ?description ~schema ~parse ?(strict = true) () =
  { name; description; schema; parse; strict }

(** Convert a typed tool to a raw Tool.t for API calls *)
let to_tool t =
  Tool.make_function
    ?description:t.description
    ~parameters:t.schema
    ~strict:t.strict
    t.name

(** Convert multiple typed tools to raw Tool.t list *)
let to_tools tools = List.map to_tool tools

(** {1 Tool Call Handling} *)

(** Check if a tool call matches this typed tool *)
let matches t (call : Tool.tool_call) =
  call.function_.name = t.name

(** Parse a tool call's arguments using this tool's parser.
    Returns None if the tool name doesn't match. *)
let parse_call t (call : Tool.tool_call) =
  if matches t call then
    try
      let json = Yojson.Safe.from_string call.function_.arguments in
      Some (t.parse json)
    with _ -> None
  else None

(** Parse a tool call's arguments, raising an exception on failure.
    @raise Failure if tool name doesn't match or parsing fails *)
let parse_call_exn t (call : Tool.tool_call) =
  if not (matches t call) then
    failwith (Printf.sprintf "Tool name mismatch: expected %s, got %s"
                t.name call.function_.name)
  else
    let json = Yojson.Safe.from_string call.function_.arguments in
    t.parse json

(** Get the tool call ID for responding *)
let call_id (call : Tool.tool_call) = call.id

(** {1 Multi-Tool Handling}

    For handling multiple tools with pattern matching, define a variant type
    and use these helpers. *)

(** A parsed tool call with its ID *)
type 'a parsed_call = {
  id : string;
  params : 'a;
}

(** Parse a tool call and include the call ID *)
let parse_call_with_id t call =
  match parse_call t call with
  | Some params -> Some { id = call.id; params }
  | None -> None

(** {1 Toolset}

    A collection of tools that can be dispatched dynamically.
    Use this when you want runtime tool dispatch instead of
    compile-time pattern matching. *)

(** Alias for the typed tool type *)
type 'a typed_tool = 'a t

module Toolset = struct
  (** A handler that can process a tool call and return a string result *)
  type handler = Tool.tool_call -> string option

  (** A toolset is a list of tools and their handlers *)
  type t = {
    tools : Tool.tool list;
    handlers : handler list;
  }

  (** Empty toolset *)
  let empty = { tools = []; handlers = [] }

  (** Add a typed tool with its handler to the toolset.
      The handler receives parsed params and returns the result string. *)
  let add (typed_tool : 'a typed_tool) (handle : 'a -> string) (ts : t) : t =
    let raw_tool = to_tool typed_tool in
    let handler call =
      match parse_call typed_tool call with
      | Some params -> Some (handle params)
      | None -> None
    in
    { tools = ts.tools @ [raw_tool];
      handlers = ts.handlers @ [handler] }

  (** Get raw tools for API calls *)
  let to_tools ts = ts.tools

  (** Handle a tool call, trying each handler until one succeeds *)
  let handle ts call =
    let rec try_handlers = function
      | [] -> None
      | h :: rest ->
        match h call with
        | Some result -> Some result
        | None -> try_handlers rest
    in
    try_handlers ts.handlers

  (** Handle a tool call and create a tool response message *)
  let handle_to_message ts call =
    match handle ts call with
    | Some result -> Some (Message.tool ~tool_call_id:call.id result)
    | None -> None

  (** Handle all tool calls from a response *)
  let handle_all ts calls =
    List.filter_map (handle_to_message ts) calls
end
