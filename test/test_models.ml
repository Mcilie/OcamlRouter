(** Tests for Models API *)

let test_list_models () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  match Openrouter.Models.list ~sw ~env client with
  | Ok response ->
    let count = List.length response.data in
    Printf.printf "Found %d models\n" count;
    assert (count > 0);

    (* Check first model has required fields *)
    let first = List.hd response.data in
    Printf.printf "First model: %s (%s)\n" first.name first.id;
    assert (String.length first.id > 0);
    assert (String.length first.name > 0);

    print_endline "test_list_models: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let test_count_models () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  match Openrouter.Models.count ~sw ~env client with
  | Ok response ->
    let count = response.data.count in
    Printf.printf "Model count: %d\n" count;
    assert (count > 0);
    print_endline "test_count_models: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let test_find_by_id () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  match Openrouter.Models.list ~sw ~env client with
  | Ok response ->
    let models = response.data in

    (* Find GPT-4o-mini which should exist *)
    (match Openrouter.Models.find_by_id models "openai/gpt-4o-mini" with
     | Some model ->
       Printf.printf "Found model: %s\n" model.name;
       print_endline "test_find_by_id: PASSED"
     | None ->
       print_endline "openai/gpt-4o-mini not found, trying another model";
       (* Just check that find works with first model *)
       let first = List.hd models in
       match Openrouter.Models.find_by_id models first.id with
       | Some _ -> print_endline "test_find_by_id: PASSED"
       | None -> assert false)
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let test_filter_by_modality () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  match Openrouter.Models.list ~sw ~env client with
  | Ok response ->
    let models = response.data in

    (* Filter for text input models *)
    let text_models = Openrouter.Models.filter_by_input_modality models "text" in
    Printf.printf "Models with text input: %d\n" (List.length text_models);
    assert (List.length text_models > 0);

    (* Filter for image input models *)
    let image_models = Openrouter.Models.filter_by_input_modality models "image" in
    Printf.printf "Models with image input: %d\n" (List.length image_models);

    print_endline "test_filter_by_modality: PASSED"
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e);
    assert false

let () =
  print_endline "=== Running Models API Tests ===\n";
  test_list_models ();
  print_newline ();
  test_count_models ();
  print_newline ();
  test_find_by_id ();
  print_newline ();
  test_filter_by_modality ();
  print_endline "\n=== All Models Tests Passed ==="
