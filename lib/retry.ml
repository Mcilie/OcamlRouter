(** Retry logic with exponential backoff *)

type backoff = {
  initial_interval_ms : int;
  max_interval_ms : int;
  exponent : float;
  max_elapsed_ms : int;
}

type config = {
  backoff : backoff;
  retry_connection_errors : bool;
  max_retries : int;
}

let default_backoff =
  {
    initial_interval_ms = 500;
    max_interval_ms = 60000;
    exponent = 1.5;
    max_elapsed_ms = 300000;
  }

let default = { backoff = default_backoff; retry_connection_errors = true; max_retries = 3 }

let no_retry =
  {
    backoff = { default_backoff with max_elapsed_ms = 0 };
    retry_connection_errors = false;
    max_retries = 0;
  }

let is_retryable_status = function
  | 429 | 500 | 502 | 503 | 504 -> true
  | _ -> false

let calculate_delay backoff attempt =
  let base_delay =
    float_of_int backoff.initial_interval_ms
    *. (backoff.exponent ** float_of_int attempt)
  in
  let jitter = Random.float (base_delay *. 0.1) in
  let delay = base_delay +. jitter in
  min (int_of_float delay) backoff.max_interval_ms

let with_retry ~clock config f =
  let start_time = Eio.Time.now clock in
  let rec loop attempt =
    match f () with
    | Ok _ as result -> result
    | Error err when Errors.is_retryable err && attempt < config.max_retries ->
        let elapsed_ms =
          int_of_float ((Eio.Time.now clock -. start_time) *. 1000.0)
        in
        if elapsed_ms >= config.backoff.max_elapsed_ms then Error err
        else
          let delay_ms = calculate_delay config.backoff attempt in
          Eio.Time.sleep clock (float_of_int delay_ms /. 1000.0);
          loop (attempt + 1)
    | Error _ as result -> result
  in
  loop 0
