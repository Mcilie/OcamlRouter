(** Tests for Completions API *)

let test_generate_completion () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  let prompt = Openrouter.Completions.String "Once upon a time" in

  match Openrouter.Completions.generate ~sw ~env client
    ~model:"openai/gpt-4o-mini"
    ~max_tokens:50
    ~prompt
    ()
  with
  | Ok response ->
    Printf.printf "Model: %s\n" response.model;
    Printf.printf "Choices: %d\n" (List.length response.choices);

    (match Openrouter.Completions.get_text response with
     | Some text ->
       Printf.printf "Generated text: %s\n" text
     | None ->
       print_endline "No text generated");

    (match response.usage with
     | Some usage ->
       Printf.printf "Tokens - Prompt: %d, Completion: %d, Total: %d\n"
         usage.prompt_tokens usage.completion_tokens usage.total_tokens
     | None -> ());

    assert (List.length response.choices > 0);
    print_endline "\ntest_generate_completion: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let () =
  print_endline "=== Running Completions API Tests ===\n";
  test_generate_completion ();
  print_endline "\n=== All Completions Tests Passed ==="
