(lang dune 3.0)

(name openrouter)

(generate_opam_files true)

(package
 (name openrouter)
 (synopsis "OCaml SDK for OpenRouter API")
 (description "A type-safe OCaml client library for the OpenRouter API using Eio for effects-based concurrency")
 (depends
  (ocaml (>= 5.1.0))
  (eio (>= 1.0))
  eio_main
  cohttp-eio
  (yojson (>= 2.0))
  (ppx_yojson_conv (>= 0.16))
  uri
  base64
  (alcotest :with-test)))
