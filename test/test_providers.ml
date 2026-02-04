(** Tests for Providers API *)

let test_list_providers () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  match Openrouter.Providers.list ~sw ~env client with
  | Ok response ->
    let providers = response.data in
    let count = List.length providers in
    Printf.printf "Found %d providers\n" count;
    assert (count > 0);

    (* Print first 5 providers *)
    print_endline "\nFirst 5 providers:";
    List.iteri (fun i (p : Openrouter.Providers.t) ->
      if i < 5 then
        Printf.printf "  %d. %s (%s)\n" (i + 1) p.name p.slug
    ) providers;

    (* Find OpenAI *)
    print_endline "\nLooking for OpenAI:";
    (match Openrouter.Providers.find_by_slug providers "openai" with
     | Some p ->
       Printf.printf "  Found: %s\n" p.name;
       Printf.printf "  Privacy Policy: %s\n"
         (Option.value p.privacy_policy_url ~default:"N/A");
       Printf.printf "  Terms of Service: %s\n"
         (Option.value p.terms_of_service_url ~default:"N/A");
       Printf.printf "  Status Page: %s\n"
         (Option.value p.status_page_url ~default:"N/A")
     | None ->
       print_endline "  OpenAI not found");

    print_endline "\ntest_list_providers: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let () =
  print_endline "=== Running Providers API Tests ===\n";
  test_list_providers ();
  print_endline "\n=== All Providers Tests Passed ==="
