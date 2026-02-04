# OpenRouter OCaml SDK - TODO

## Missing Endpoints

### Models
- [x] `GET /models` - List all available models
- [x] `GET /models/count` - Get total model count
- [x] `GET /models/{author}/{slug}/endpoints` - List endpoints for a model

### Completions
- [x] `POST /completions` - Text completions (non-chat)

### Embeddings
- [x] `POST /embeddings` - Generate embeddings
- [x] `GET /embeddings/models` - List embedding models

### Account & Usage
- [x] `GET /credits` - Check remaining credits
- [x] `GET /generation` - Get generation metadata by ID
- [ ] `GET /activity` - Analytics/usage data (requires provisioning key)

### Providers
- [x] `GET /providers` - List all providers

### API Keys Management
- [ ] `GET /keys` - List API keys
- [ ] `POST /keys` - Create API key
- [ ] `GET /keys/{key_id}` - Get API key details
- [ ] `PATCH /keys/{key_id}` - Update API key
- [ ] `POST /keys/{key_id}/disable` - Disable API key
- [ ] `DELETE /keys/{key_id}` - Delete API key

### Guardrails
- [ ] `GET /guardrails` - List guardrails
- [ ] `POST /guardrails` - Create guardrail
- [ ] `GET /guardrails/{id}` - Get guardrail details
- [ ] `PATCH /guardrails/{id}` - Update guardrail
- [ ] `DELETE /guardrails/{id}` - Delete guardrail

### OAuth
- [ ] `POST /auth/keys` - Exchange auth code for API key (PKCE flow)

---

## Missing Chat Features

### Content Types
- [x] Audio content (input/output)
- [x] Video content (input)
- [ ] Cache control directives

### Response Formatting
- [x] JSON mode (`response_format: { type: "json_object" }`)
- [x] JSON schema validation
- [x] Text format option

### Advanced Parameters
- [x] `logit_bias` - Bias specific tokens
- [x] `logprobs` - Return log probabilities
- [x] `top_logprobs` - Number of top logprobs to return
- [x] `modalities` - Output modalities (text, audio)

### Tracking & Metadata
- [x] `session_id` - Group related requests
- [x] `metadata` - Custom key-value pairs per request

### Other
- [ ] Plugins support
- [ ] Debug mode
- [ ] Image configuration per provider

---

## Missing Error Types

- [ ] 402 Payment Required
- [ ] 408 Request Timeout
- [ ] 413 Payload Too Large
- [ ] 422 Unprocessable Entity
- [ ] 524 Edge Network Timeout
- [ ] 529 Provider Overloaded

---

## Tests

- [x] Tests for models API
- [x] Tests for completions API
- [x] Tests for embeddings API
- [x] Tests for credits API
- [x] Tests for generation API
- [x] Tests for providers API
- [ ] Tests for API keys API
- [ ] Tests for guardrails API
- [x] Tests for new chat features
- [ ] Tests for error types
- [x] Tests for pipeline builder
- [x] Tests for typed tool system

---

## OCaml-Idiomatic Enhancements

### Composable Pipelines (Priority 1)
- [x] Pipeline builder module with `|>` composition
- [x] `Pipeline.prompt` - Start a pipeline from a string
- [x] `Pipeline.system` - Add system message to pipeline
- [x] `Pipeline.user` - Add user message to pipeline
- [x] `Pipeline.assistant` - Add assistant message to pipeline
- [x] `Pipeline.model` - Set model
- [x] `Pipeline.temperature` - Set temperature
- [x] `Pipeline.max_tokens` - Set max tokens
- [x] `Pipeline.tools` - Add tools
- [x] `Pipeline.json_mode` - Enable JSON response format
- [x] `Pipeline.json_schema` - Set JSON schema constraint
- [x] `Pipeline.run` - Execute pipeline (non-streaming)
- [x] `Pipeline.run_stream` - Execute pipeline (streaming)
- [x] Additional helpers: `top_p`, `top_k`, `stop`, `frequency_penalty`, `presence_penalty`, `seed`, `logit_bias`, `logprobs`, `session_id`, `metadata`, `modalities`, `reasoning`, `provider`, `route`, `tool_choice`, `force_tool`

### Typed Tool System (Priority 2)
- [x] Tool definition as OCaml types
- [x] Schema builder helpers (`Typed_tool.Schema` module)
- [x] Pattern matching on tool responses (`matches`, `parse_call`, `parse_call_exn`)
- [x] Toolset for runtime dispatch (`Typed_tool.Toolset`)
- [x] Auto-generate response messages (`Toolset.handle_all`)
- [ ] PPX for auto-generating schema from types (future enhancement)
- [ ] Compile-time handler coverage checks with GADTs (future enhancement)

### Agent Framework (Priority 3)
- [ ] Agent loop with automatic tool execution
- [ ] Step-by-step execution with pattern matching
- [ ] Customizable tool handlers
- [ ] Conversation state management
- [ ] Max iterations / budget controls
