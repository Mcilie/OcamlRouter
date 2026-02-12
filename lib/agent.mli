(** Agent framework: match-driven decisioning and auto-tool loops *)

module Context : sig
  type t = {
    sw : Eio.Switch.t;
    env : Eio_unix.Stdenv.base;
    client : Client.t;
    clock : float Eio.Time.clock_ty Eio.Resource.t;
  }

  val make : sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> client:Client.t -> t
end

module Budget : sig
  type t = {
    max_steps : int;
    max_tool_rounds : int;
    max_total_tool_calls : int;
    max_elapsed_s : float option;
  }

  val default : t
end

module Error : sig
  type framework =
    | Api of Errors.t
    | Decision_parse of string
    | Tool_parse of string
    | Tool_handler of string
    | Budget_exhausted of [ `Steps | `Tool_rounds | `Tool_calls | `Elapsed ]
    | Protocol of string

  type 'e t = Framework of framework | User of 'e
end

module Observation : sig
  type t =
    | Content of string
    | Tool_calls of Tool.tool_call list
    | Content_and_tool_calls of string * Tool.tool_call list
    | Refusal of string
    | Empty

  val of_response : Chat.response -> t
end

module Decision : sig
  type t = { action : string; args : Yojson.Safe.t }

  type parse_error =
    | No_call
    | Multiple_calls
    | Wrong_tool of string
    | Invalid_args of string

  val tool :
    ?name:string ->
    ?strict:bool ->
    actions:string list ->
    ?args_schema:Yojson.Safe.t ->
    unit ->
    t Typed_tool.t

  val parse_tool_calls :
    t Typed_tool.t ->
    Tool.tool_call list ->
    (t, parse_error) result

  val decode :
    (string -> Yojson.Safe.t -> ('a, string) result) ->
    t ->
    ('a, parse_error) result
end

module Tools : sig
  type tool_error = { tool : string; message : string }

  type ('ctx,'params) handler =
    'ctx -> 'params -> (string, tool_error) result

  type 'ctx t

  val empty : 'ctx t

  val add :
    'params Typed_tool.t ->
    ('ctx,'params) handler ->
    'ctx t -> 'ctx t

  val to_tools : 'ctx t -> Tool.tool list

  type policy = {
    on_unhandled : [ `Fail | `Ignore | `Message ];
    on_error : [ `Fail | `Message ];
    error_to_string : tool_error -> string;
  }

  val default_policy : policy

  val handle_all :
    ?policy:policy ->
    'ctx t -> 'ctx -> Tool.tool_call list ->
    (Message.t list, tool_error) result
end

module Loop : sig
  type ('s,'r,'e) outcome =
    | Continue of 's
    | Done of 'r
    | Fail of 'e Error.t

  val run :
    ctx:Context.t ->
    budget:Budget.t ->
    step:('s -> ('s,'r,'e) outcome) ->
    init:'s ->
    ('r, 'e Error.t) result
end

module Auto : sig
  type model_call =
    ctx:Context.t ->
    tools:Tool.tool list ->
    messages:Message.t list ->
    (Chat.response, Errors.t) result

  type result = {
    response : Chat.response;
    messages : Message.t list;
  }

  val run :
    ctx:Context.t ->
    budget:Budget.t ->
    tools:Context.t Tools.t ->
    model:string ->
    messages:Message.t list ->
    ?policy:Tools.policy ->
    ?model_call:model_call ->
    unit ->
    (result, Error.framework) Stdlib.result
end
