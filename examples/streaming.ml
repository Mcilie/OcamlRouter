(** Streaming example *)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  (* Create client from environment variable *)
  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY environment variable"
  in

  (* Create messages *)
  let messages = [
    Openrouter.Message.user "Write a short poem about OCaml.";
  ] in

  (* Send streaming request *)
  match Openrouter.Chat.send_streaming ~sw ~env client
    ~model:"openai/gpt-4o-mini"
    ~messages
    ()
  with
  | Ok stream ->
    print_string "Response: ";
    flush stdout;
    Openrouter.Chat.iter_stream (fun chunk ->
      match Openrouter.Streaming.get_content chunk with
      | Some text ->
        print_string text;
        flush stdout
      | None -> ()
    ) stream;
    print_newline ()
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e)
