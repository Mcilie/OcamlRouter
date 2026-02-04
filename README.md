# openrouter-ocaml

A type-safe OCaml SDK for the [OpenRouter API](https://openrouter.ai/), featuring a composable pipeline API that embraces OCaml idioms.

## Why?

OpenRouter provides unified access to 200+ LLMs (GPT-4, Claude, Llama, Mistral, etc.) through a single API. This library brings that to OCaml with:

- **Type safety** — Catch errors at compile time, not runtime
- **Composable pipelines** — Build requests with `|>` like idiomatic OCaml
- **Direct-style async** — Uses Eio (OCaml 5.0+) for clean, readable concurrent code
- **Full API coverage** — Chat, streaming, tools, embeddings, and more

## Installation

Requires OCaml 5.1+ (for Eio).

```bash
opam pin add openrouter https://github.com/yourusername/openrouter-ocaml.git
```

Or add to your `dune-project`:

```lisp
(depends
  (openrouter (>= 0.1.0)))
```

## Quick Start

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let client = Openrouter.from_env () |> Option.get in

  let result =
    Openrouter.Chat.send ~sw ~env client
      ~model:"openai/gpt-5"
      ~messages:[Openrouter.Message.user "Hello!"]
      ()
  in

  match result with
  | Ok response ->
    (match Openrouter.Chat.get_content response with
     | Some text -> print_endline text
     | None -> ())
  | Error e ->
    prerr_endline (Openrouter.Errors.to_string e)
```

Set your API key:

```bash
export OPENROUTER_API_KEY=sk-or-...
```

## The Pipeline API

The standout feature is the composable pipeline builder. Instead of passing many optional arguments, chain operations naturally:

```ocaml
open Openrouter.Pipeline

let result =
  "Explain monads in simple terms"
  |> prompt
  |> system "You are a friendly teacher. Be concise."
  |> model "anthropic/claude-4.5-sonnet"
  |> temperature 0.7
  |> max_tokens 200
  |> run ~sw ~env client
```

### JSON Mode (Structured Output)

```ocaml
let person_schema = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("name", `Assoc [("type", `String "string")]);
    ("age", `Assoc [("type", `String "integer")]);
  ]);
  ("required", `List [`String "name"; `String "age"]);
  ("additionalProperties", `Bool false);
] in

"Generate a fictional person"
|> prompt
|> model "openai/gpt-5"
|> json_mode ~name:"person" ~strict:true person_schema
|> run ~sw ~env client
(* Returns: {"name": "Alice", "age": 28} *)
```

### Streaming

```ocaml
"Write a short story"
|> prompt
|> model "openai/gpt-5"
|> run_stream ~sw ~env client
|> Result.map (fun stream ->
     Openrouter.Chat.iter_stream (fun chunk ->
       match Openrouter.Streaming.get_content chunk with
       | Some text -> print_string text; flush stdout
       | None -> ()
     ) stream)
```

### Multi-turn Conversations

```ocaml
let conversation = [
  Openrouter.Message.system "You are a math tutor.";
  Openrouter.Message.user "What is 2+2?";
  Openrouter.Message.assistant "4";
  Openrouter.Message.user "What about 3+3?";
] in

conversation
|> messages
|> model "openai/gpt-5"
|> run ~sw ~env client
```

### Tool Use

```ocaml
let weather_tool = Openrouter.Tool.make_function
  ~description:"Get current weather"
  ~parameters:(`Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("location", `Assoc [("type", `String "string")])
    ]);
    ("required", `List [`String "location"]);
  ])
  "get_weather"
in

"What's the weather in Tokyo?"
|> prompt
|> model "openai/gpt-5"
|> tools [weather_tool]
|> run ~sw ~env client
```

## API Coverage

### Chat Completions
- Non-streaming and streaming responses
- Tool/function calling
- JSON mode and schema-constrained output
- Vision (images), audio, and video input
- Reasoning models support
- All sampling parameters (temperature, top_p, top_k, etc.)
- Logit bias, logprobs
- Session tracking and metadata

### Models
- List all available models
- Get model count
- Get model endpoints
- Filter by modality, parameters

### Embeddings
- Generate embeddings
- List embedding models
- Cosine similarity helper

### Account
- Check credits balance
- Get generation metadata

### Providers
- List all providers

## Configuration

```ocaml
(* From environment variable *)
let client = Openrouter.from_env () |> Option.get

(* Explicit API key *)
let client = Openrouter.create ~api_key:"sk-or-..." ()

(* With options *)
let client = Openrouter.create
  ~api_key:"sk-or-..."
  ~base_url:"https://openrouter.ai/api/v1"
  ~timeout:30.0
  ~app_name:"MyApp"
  ~app_url:"https://myapp.com"
  ()
```

## Error Handling

All API calls return `(result, Openrouter.Errors.error) result`:

```ocaml
match result with
| Ok response -> (* handle success *)
| Error (Openrouter.Errors.Rate_limited data) ->
    Printf.eprintf "Rate limited: %s\n" data.message
| Error (Openrouter.Errors.Unauthorized _) ->
    prerr_endline "Invalid API key"
| Error e ->
    Printf.eprintf "Error: %s\n" (Openrouter.Errors.to_string e)
```

## Running Examples

```bash
# Set up environment
export OPENROUTER_API_KEY=sk-or-...

# Run examples
dune exec examples/simple_chat.exe
dune exec examples/streaming.exe
dune exec examples/tool_use.exe
dune exec examples/pipeline_demo.exe
```

## Requirements

- OCaml 5.1+ (for Eio effects)
- `curl` available in PATH (used for HTTP)

### Important: ulimit on macOS

Eio requires a reasonable file descriptor limit. Before running:

```bash
ulimit -n 10240
```

The library includes a runtime check (`Openrouter.Env_check.check_ulimit ()`) that warns if the limit is problematic.

## Dependencies

- `eio`, `eio_main` — Direct-style concurrency
- `yojson`, `ppx_yojson_conv` — JSON serialization
- `uri` — URL handling
- `base64` — Image encoding

## License

MIT

## Contributing

Contributions welcome! Please open an issue or PR on GitHub.
