(** SDK configuration *)

let default_base_url = "https://openrouter.ai/api/v1"
let default_timeout = 120.0

type t = {
  base_url : string;
  timeout : float;
  app_name : string option;
  app_url : string option;
}

let create ?(base_url = default_base_url) ?(timeout = default_timeout) ?app_name
    ?app_url () =
  { base_url; timeout; app_name; app_url }

let default = create ()

let with_base_url base_url t = { t with base_url }
let with_timeout timeout t = { t with timeout }
let with_app_name app_name t = { t with app_name = Some app_name }
let with_app_url app_url t = { t with app_url = Some app_url }

let extra_headers t =
  let headers = [] in
  let headers =
    match t.app_name with
    | Some name -> ("X-Title", name) :: headers
    | None -> headers
  in
  let headers =
    match t.app_url with
    | Some url -> ("HTTP-Referer", url) :: headers
    | None -> headers
  in
  headers
