(** Simple chat example *)

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
    Openrouter.Message.system "You are a helpful assistant.";
    Openrouter.Message.user "Hello! What's 2 + 2?";
  ] in

  (* Send request *)
  match Openrouter.Chat.send ~sw ~env client
    ~model:"openai/gpt-4o-mini"
    ~messages
    ()
  with
  | Ok response ->
    (match Openrouter.Chat.get_content response with
     | Some content -> print_endline content
     | None -> print_endline "No content in response")
  | Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e)
