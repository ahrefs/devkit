let bracket resource destroy k = Std.finally (fun () -> destroy resource) k resource

let wrapped acc result k =
  let r = ref None in
  let () = Std.finally (fun () -> r := Some (result acc)) k acc in
  match !r with
  | None -> assert false
  | Some x -> x

let with_open_in_txt name = bracket (open_in name) close_in_noerr
let with_open_out_txt name = bracket (open_out name) close_out_noerr
let with_open_in_bin name = bracket (open_in_bin name) close_in_noerr
let with_open_out_bin name = bracket (open_out_bin name) close_out_noerr
let with_open_out_temp_file ?temp_dir ~mode = bracket (Filename.open_temp_file ~mode ?temp_dir "dvkt" "tmp") (fun (_,ch) -> close_out_noerr ch)
let with_open_out_temp_bin k = with_open_out_temp_file ~mode:[Open_binary] k
let with_open_out_temp_txt k = with_open_out_temp_file ~mode:[Open_text] k

let wrapped_output io = wrapped io IO.close_out
let wrapped_outs k = wrapped_output (IO.output_string ()) k
let with_input io = bracket io IO.close_in
let with_input_bin name k = with_open_in_bin name (fun ch -> k (IO.input_channel ch))
let with_input_txt name k = with_open_in_txt name (fun ch -> k (IO.input_channel ch))
let with_output io = bracket io IO.close_out
let with_output_bin name k = with_open_out_bin name (fun ch -> bracket (IO.output_channel ch) IO.flush k)
let with_output_txt name k = with_open_out_txt name (fun ch -> bracket (IO.output_channel ch) IO.flush k)

let with_opendir dir = bracket (Unix.opendir dir) Unix.closedir

(* token bucket
   https://en.wikipedia.org/wiki/Token_bucket *)
module Rate_limit = struct
  type t =
    | None
    | RL of {
      mutable tokens: float;
      mutable count_silenced: int;
      mutable last_update: float;
      capacity: float;
      rate: float; (** new tokens/sec *)
    }

  let none = None

  let create ?burst_capacity ~allowed_per_sec () : t =
    if classify_float allowed_per_sec <> FP_normal || allowed_per_sec <= 0. then
      invalid_arg "Rate_limit.create: allowed_per_sec must be finite and positive";
    let capacity = match burst_capacity with
      | Some n ->
        if n < 1 then invalid_arg "Rate_limit.create: burst capacity must be >= 1";
        float n
      | None ->
        (* default: burst of 5sec worth of tokens *)
        max 1. @@ min max_float @@ allowed_per_sec *. 5.
    in
    RL {
      tokens=capacity; last_update=Time.now(); count_silenced=0; capacity;
      rate=allowed_per_sec;
    }

  let take_rate_limited_count = function
    | None -> 0
    | RL rl ->
        let n = rl.count_silenced in
        rl.count_silenced <- 0;
        n

  let attempt = function
    | None -> true
    | RL rl ->
      let now = Time.now() in

      if now > rl.last_update then (
        rl.tokens <- min rl.capacity
          (rl.tokens +. rl.rate *. (now -. rl.last_update));
        rl.last_update <- now;
      );

      if rl.tokens >= 1. then (
        rl.tokens <- rl.tokens -. 1.;
        true
      ) else (
        rl.count_silenced <- 1 + rl.count_silenced;
        false
      )
end
