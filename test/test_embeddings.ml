(** Tests for Embeddings API *)

let test_list_embedding_models () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  match Openrouter.Embeddings.list_models ~sw ~env client with
  | Ok response ->
    let models = response.data in
    Printf.printf "Found %d embedding models\n" (List.length models);

    (* Print first 5 embedding models *)
    print_endline "\nFirst 5 embedding models:";
    List.iteri (fun i (m : Openrouter.Models.t) ->
      if i < 5 then
        Printf.printf "  %d. %s (%s)\n" (i + 1) m.name m.id
    ) models;

    assert (List.length models > 0);
    print_endline "\ntest_list_embedding_models: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let test_generate_embedding () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  let input = Openrouter.Embeddings.Single "Hello, world!" in

  match Openrouter.Embeddings.generate ~sw ~env client
    ~model:"thenlper/gte-base"
    ~input
    ()
  with
  | Ok response ->
    Printf.printf "Model: %s\n" response.model;
    Printf.printf "Provider: %s\n" (Option.value response.provider ~default:"N/A");
    Printf.printf "Tokens used: %d\n" response.usage.total_tokens;

    (match Openrouter.Embeddings.get_embedding response with
     | Some embedding ->
       Printf.printf "Embedding dimension: %d\n" (List.length embedding);
       Printf.printf "First 5 values: [%s]\n"
         (String.concat ", "
            (List.map (Printf.sprintf "%.4f")
               (List.filteri (fun i _ -> i < 5) embedding)))
     | None ->
       print_endline "No embedding returned");

    assert (List.length response.data > 0);
    print_endline "\ntest_generate_embedding: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let test_generate_multiple_embeddings () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  let input = Openrouter.Embeddings.Multiple [
    "The quick brown fox";
    "jumps over the lazy dog"
  ] in

  match Openrouter.Embeddings.generate ~sw ~env client
    ~model:"thenlper/gte-base"
    ~input
    ()
  with
  | Ok response ->
    Printf.printf "Generated %d embeddings\n" (List.length response.data);

    let embeddings = Openrouter.Embeddings.get_embeddings response in
    assert (List.length embeddings = 2);

    (* Calculate similarity between the two sentences *)
    let e1 = List.nth embeddings 0 in
    let e2 = List.nth embeddings 1 in
    let similarity = Openrouter.Embeddings.cosine_similarity e1 e2 in
    Printf.printf "Cosine similarity: %.4f\n" similarity;

    print_endline "\ntest_generate_multiple_embeddings: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let () =
  print_endline "=== Running Embeddings API Tests ===\n";
  test_list_embedding_models ();
  print_newline ();
  test_generate_embedding ();
  print_newline ();
  test_generate_multiple_embeddings ();
  print_endline "\n=== All Embeddings Tests Passed ==="
