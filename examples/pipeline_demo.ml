(** Demonstration of the composable Pipeline API

    Run with:
      eval $(opam env --switch=default) && export $(cat .env | xargs) && \
      ulimit -n 10240 && dune exec examples/pipeline_demo.exe
*)

open Openrouter.Pipeline

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client =
    match Openrouter.from_env () with
    | Some c -> c
    | None -> failwith "Please set OPENROUTER_API_KEY"
  in

  print_endline "=== Pipeline API Demo ===\n";

  (* Example 1: Simple one-liner *)
  print_endline "1. Simple prompt:";
  let result =
    "What is the meaning of life? One sentence only."
    |> prompt
    |> model "openai/gpt-4o-mini"
    |> max_tokens 50
    |> run ~sw ~env client
  in
  (match get_content result with
   | Some text -> Printf.printf "   %s\n\n" text
   | None -> print_endline "   (no response)");

  (* Example 2: With system message and parameters *)
  print_endline "2. With system message and temperature:";
  let result =
    "Write a haiku about programming"
    |> prompt
    |> system "You are a creative poet who loves technology."
    |> model "openai/gpt-4o-mini"
    |> temperature 0.9
    |> max_tokens 100
    |> run ~sw ~env client
  in
  (match get_content result with
   | Some text -> Printf.printf "   %s\n\n" text
   | None -> print_endline "   (no response)");

  (* Example 3: JSON mode *)
  print_endline "3. JSON mode:";
  let result =
    "Generate a fictional person with name, age, and hobby"
    |> prompt
    |> system "Respond with valid JSON only."
    |> model "openai/gpt-4o-mini"
    |> json_mode
    |> max_tokens 100
    |> run ~sw ~env client
  in
  (match get_content result with
   | Some text -> Printf.printf "   %s\n\n" text
   | None -> print_endline "   (no response)");

  (* Example 4: Structured output with JSON schema *)
  print_endline "4. Structured output with schema:";
  let weather_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("location", `Assoc [("type", `String "string")]);
      ("temperature_celsius", `Assoc [("type", `String "number")]);
      ("conditions", `Assoc [
        ("type", `String "string");
        ("enum", `List [`String "sunny"; `String "cloudy"; `String "rainy"; `String "snowy"])
      ]);
    ]);
    ("required", `List [`String "location"; `String "temperature_celsius"; `String "conditions"]);
    ("additionalProperties", `Bool false);
  ] in
  let result =
    "What's the weather like in Tokyo right now? (make up realistic values)"
    |> prompt
    |> model "openai/gpt-4o-mini"
    |> json_schema ~name:"weather" ~strict:true weather_schema
    |> run ~sw ~env client
  in
  (match get_content result with
   | Some text -> Printf.printf "   %s\n\n" text
   | None -> print_endline "   (no response)");

  (* Example 5: Streaming *)
  print_endline "5. Streaming response:";
  print_string "   ";
  let result =
    "Count slowly from 1 to 10, with a brief pause between each number."
    |> prompt
    |> model "openai/gpt-4o-mini"
    |> max_tokens 100
    |> run_stream ~sw ~env client
  in
  (match result with
   | Ok stream ->
     Openrouter.Chat.iter_stream (fun chunk ->
       match Openrouter.Streaming.get_content chunk with
       | Some text -> print_string text; flush stdout
       | None -> ()
     ) stream;
     print_endline ""
   | Error e ->
     Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e));

  (* Example 6: Multi-turn conversation *)
  print_endline "\n6. Multi-turn conversation:";
  let conversation = [
    Openrouter.Message.system "You are a helpful math tutor. Be concise.";
    Openrouter.Message.user "What is the Pythagorean theorem?";
    Openrouter.Message.assistant "The Pythagorean theorem states that in a right triangle, a² + b² = c², where c is the hypotenuse.";
    Openrouter.Message.user "Can you give me an example with numbers?";
  ] in
  let result =
    conversation
    |> messages
    |> model "openai/gpt-4o-mini"
    |> max_tokens 100
    |> run ~sw ~env client
  in
  (match get_content result with
   | Some text -> Printf.printf "   %s\n\n" text
   | None -> print_endline "   (no response)");

  print_endline "=== Demo Complete ==="
