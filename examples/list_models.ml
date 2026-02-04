(** Example: List available models *)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  (* Get model count *)
  (match Openrouter.Models.count ~sw ~env client with
   | Ok response ->
     Printf.printf "Total models available: %d\n\n" response.data.count
   | Error e ->
     Printf.eprintf "Error getting count: %s\n" (Openrouter.Errors.to_string e));

  (* List all models *)
  match Openrouter.Models.list ~sw ~env client with
  | Ok response ->
    let models = response.data in

    (* Show first 10 models *)
    print_endline "=== First 10 Models ===";
    List.iteri (fun i (m : Openrouter.Models.t) ->
      if i < 10 then
        Printf.printf "%d. %s (%s)\n   Context: %s, Pricing: $%s/1M prompt, $%s/1M completion\n"
          (i + 1)
          m.name
          m.id
          (match m.context_length with Some c -> string_of_int c | None -> "N/A")
          m.pricing.prompt
          m.pricing.completion
    ) models;

    (* Filter for vision models *)
    print_endline "\n=== Vision Models (first 5) ===";
    let vision_models = Openrouter.Models.filter_by_input_modality models "image" in
    List.iteri (fun i (m : Openrouter.Models.t) ->
      if i < 5 then
        Printf.printf "%d. %s\n" (i + 1) m.name
    ) vision_models;

    (* Find a specific model *)
    print_endline "\n=== Looking up GPT-4o ===";
    (match Openrouter.Models.find_by_id models "openai/gpt-4o" with
     | Some m ->
       Printf.printf "Found: %s\n" m.name;
       Printf.printf "Description: %s\n"
         (match m.description with Some d -> d | None -> "N/A");
       Printf.printf "Supported parameters: %s\n"
         (String.concat ", " m.supported_parameters)
     | None ->
       print_endline "Model not found")

  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e)
