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

(* token bucket, domain-safe.
   https://en.wikipedia.org/wiki/Token_bucket *)
module Rate_limit = struct
  type bucket = {
    tokens: float;
    last_update: float;
  }

  type t =
    | Unlimited
    | RL of {
      bucket: bucket Atomic.t; (** current state. avoid mutex for reentrancy *)
      count_silenced: int Atomic.t;
      capacity: float;
      rate: float; (** new tokens/sec *)
    }

  let unlimited = Unlimited

  let create ?(burst_factor=5) ~allowed_per_sec () : t =
    if classify_float allowed_per_sec <> FP_normal || allowed_per_sec <= 0. then
      invalid_arg "Rate_limit.create: allowed_per_sec must be finite and positive";

    if burst_factor < 1 then invalid_arg "Rate_limit.create: burst factor must be >= 1";
    let capacity = max 1. (float burst_factor *. allowed_per_sec) in
    RL {
      bucket = Atomic.make { tokens=capacity; last_update=Time.now() };
      count_silenced = Atomic.make 0;
      capacity;
      rate=allowed_per_sec;
    }

  let take_rate_limited_count = function
    | Unlimited -> 0
    | RL rl -> Atomic.exchange rl.count_silenced 0

  let rec attempt_rec now = function
    | Unlimited -> true
    | RL rl as rate_limiter ->
      let old = Atomic.get rl.bucket in
      let b =
        let time_since_last_refill = now -. old.last_update in
        if time_since_last_refill > 1e-3 then
          (* lazily refill, avoid small float precision errors *)
          let tokens = min rl.capacity (old.tokens +. rl.rate *. time_since_last_refill) in
          { tokens; last_update = now }
        else
          old
      in
      if b.tokens >= 1. then
        let ok = Atomic.compare_and_set rl.bucket old { b with tokens = b.tokens -. 1. } in
        if ok then true (* done *) else attempt_rec now rate_limiter
      else begin
        Atomic.incr rl.count_silenced;
        false
      end

  let attempt rl = attempt_rec (Time.now()) rl
end
