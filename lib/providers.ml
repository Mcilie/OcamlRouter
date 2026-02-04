(** Provider types *)

type t = {
  name : string;
  slug : string;
  privacy_policy_url : string option;
  terms_of_service_url : string option;
  status_page_url : string option;
}

let t_of_yojson json =
  let open Yojson.Safe.Util in
  {
    name = json |> member "name" |> to_string;
    slug = json |> member "slug" |> to_string;
    privacy_policy_url = json |> member "privacy_policy_url" |> to_string_option;
    terms_of_service_url = json |> member "terms_of_service_url" |> to_string_option;
    status_page_url = json |> member "status_page_url" |> to_string_option;
  }

let yojson_of_t p =
  let fields = [
    ("name", `String p.name);
    ("slug", `String p.slug);
  ] in
  let add_opt name v fields = match v with Some s -> (name, `String s) :: fields | None -> (name, `Null) :: fields in
  let fields = add_opt "privacy_policy_url" p.privacy_policy_url fields in
  let fields = add_opt "terms_of_service_url" p.terms_of_service_url fields in
  let fields = add_opt "status_page_url" p.status_page_url fields in
  `Assoc fields

type list_response = {
  data : t list;
}

let list_response_of_yojson json =
  let open Yojson.Safe.Util in
  { data = json |> member "data" |> to_list |> List.map t_of_yojson }

let yojson_of_list_response r =
  `Assoc [("data", `List (List.map yojson_of_t r.data))]

(** Find a provider by slug *)
let find_by_slug providers slug =
  List.find_opt (fun p -> p.slug = slug) providers

(** Find a provider by name *)
let find_by_name providers name =
  List.find_opt (fun p -> p.name = name) providers
