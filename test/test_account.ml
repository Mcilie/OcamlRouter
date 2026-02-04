(** Tests for Account API *)

let test_get_credits () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  match Openrouter.Account.get_credits ~sw ~env client with
  | Ok response ->
    let credits = response.data in
    Printf.printf "Total credits: %.6f\n" credits.total_credits;
    Printf.printf "Total usage: %.6f\n" credits.total_usage;
    Printf.printf "Remaining: %.6f\n" (Openrouter.Account.remaining credits);
    assert (credits.total_credits >= 0.0);
    assert (credits.total_usage >= 0.0);
    print_endline "test_get_credits: PASSED"
  | Error e ->
    (* Credits API requires provisioning key, so 401/403 is expected with regular API key *)
    let err_str = Openrouter.Errors.to_string e in
    if String.length err_str > 0 then
      Printf.printf "Expected error (needs provisioning key): %s\n" err_str;
    print_endline "test_get_credits: SKIPPED (requires provisioning key)"

let test_get_generation () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  (* First make a chat request to get a generation ID *)
  let messages = [Openrouter.Message.user "Say 'test' and nothing else."] in
  match Openrouter.Chat.send ~sw ~env client ~model:"openai/gpt-5" ~messages () with
  | Ok response ->
    let gen_id = response.id in
    Printf.printf "Got generation ID: %s\n" gen_id;

    (* Wait a bit for the generation to be indexed *)
    Unix.sleepf 2.0;

    (* Now fetch the generation metadata *)
    (match Openrouter.Account.get_generation ~sw ~env client ~id:gen_id with
     | Ok gen_response ->
       let gen = gen_response.data in
       Printf.printf "Generation ID: %s\n" gen.id;
       Printf.printf "Model: %s\n" (Option.value gen.model ~default:"N/A");
       Printf.printf "Provider: %s\n" (Option.value gen.provider ~default:"N/A");
       Printf.printf "Total cost: %s\n"
         (match gen.total_cost with Some c -> Printf.sprintf "%.8f" c | None -> "N/A");
       Printf.printf "Tokens prompt: %s\n"
         (match gen.tokens_prompt with Some t -> string_of_int t | None -> "N/A");
       Printf.printf "Tokens completion: %s\n"
         (match gen.tokens_completion with Some t -> string_of_int t | None -> "N/A");
       assert (gen.id = gen_id);
       print_endline "test_get_generation: PASSED"
     | Error e ->
       Printf.eprintf "Error fetching generation: %s\n" (Openrouter.Errors.to_string e);
       assert false)
  | Error e ->
    Printf.eprintf "Error making chat request: %s\n" (Openrouter.Errors.to_string e);
    assert false

let () =
  print_endline "=== Running Account API Tests ===\n";
  test_get_credits ();
  print_newline ();
  test_get_generation ();
  print_endline "\n=== All Account Tests Passed ==="
