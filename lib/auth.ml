(** Authentication handling *)

type t = { api_key : string }

let create api_key = { api_key }

let from_env ?(var = "OPENROUTER_API_KEY") () =
  match Sys.getenv_opt var with
  | Some key when String.length key > 0 -> Some (create key)
  | _ -> None

let authorization_header t = ("Authorization", "Bearer " ^ t.api_key)

let api_key t = t.api_key
