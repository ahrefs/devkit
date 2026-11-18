open Prelude

let log = Log.from "retry"

(** Clamped random exponential backoff for retry *)
let exp_backoff_pause ?max_delay attempt =
  let sleep = Random.int (attempt + 1) in
  let pause = 2. ** float sleep -. 1. in
  Option.map_default (min pause) pause max_delay

let wait_pause'' ?(ignore_should_exit = false) poll pause =
  log #info "will wait for %s" (Time.duration_str pause);
  let need_stamp = Time.now () +. pause in
  let rec loop () =
    match Time.now () with
    | now when now < need_stamp && (ignore_should_exit || Daemon.should_run ()) ->
      Nix.sleep (min 2. (need_stamp -. now));
      poll ();
      loop ()
    | _ -> ()
  in
  loop ()

let wait_pause' ?ignore_should_exit = wait_pause'' ?ignore_should_exit id
let wait_pause ?ignore_should_exit master = wait_pause'' ?ignore_should_exit master#poll


(* Exponentially increasing sleep pause depending of the number of attempts already made.
   Since the loop is not managed by this function, the number of attempts already made must be provided.
*)
let backoff_log ~exn ~name attempt =
  function
  | None -> log #warn ~exn "%s: aborting after %d max_retries" name attempt
  | Some pause ->
    log #warn ~exn "%s: will retry in %s (try #%d)" name (Time.duration_str pause) attempt

let exp_backoff ?(f_retry=id) ~exn ~name ?max_retries ~max_delay attempt =
  match attempt, max_retries with
  | n, Some max_retries when n > max_retries ->
    backoff_log ~exn ~name attempt None;
    Lwt.fail exn
  | attempt, _ ->
    let pause = exp_backoff_pause ~max_delay attempt in
    f_retry ();
    backoff_log ~exn ~name attempt (Some pause);
    let%lwt () = Lwt_unix.sleep pause in
    Lwt.return (attempt + 1)

let backoff_log_result to_string error ~name attempt =
  function
    | None -> log #warn "%s: aborting after %d max_retries %s" name attempt (to_string error)
  | Some pause ->
    log #warn "%s: will retry in %s (try #%d) %s" name (Time.duration_str pause) attempt (to_string error)

let exp_backoff_result ?(f_retry=id) to_string error ~name ?max_retries ~max_delay attempt =
  match attempt, max_retries with
  | n, Some max_retries when n > max_retries ->
    backoff_log_result to_string error ~name attempt None;
    Lwt.return_error error
  | attempt, _ ->
    let pause = exp_backoff_pause ~max_delay attempt in
    f_retry ();
    backoff_log_result to_string error ~name attempt (Some pause);
    let%lwt () = Lwt_unix.sleep pause in
    Lwt.return_ok (attempt + 1)

let with_exp_backoff ~name ?f_retry ?max_retries ~max_delay f =
  let rec loop f attempt =
    try%lwt
      f ()
    with
    | Daemon.ShouldExit | Lwt.Canceled as exn ->
      backoff_log ~exn ~name attempt None;
      Lwt.fail exn
    | exn ->
      let%lwt attempt = exp_backoff ?f_retry ~exn ~name ?max_retries ~max_delay attempt in
      loop f attempt
  in
  loop f 1

let exp_backoff_blocking ~master ~exn ~name ?max_retries ~max_delay attempt =
  match attempt, max_retries with
  | n, Some max_retries when n > max_retries ->
    backoff_log ~exn ~name attempt None;
    raise exn
  | attempt, _ ->
    let pause = exp_backoff_pause ~max_delay attempt in
    backoff_log ~exn ~name attempt (Some pause);
    wait_pause master pause;
    attempt + 1

let exp_backoff_blocking_no_poll ~exn ~name ?max_retries ~max_delay attempt =
  match attempt, max_retries with
  | n, Some max_retries when n > max_retries ->
    backoff_log ~exn ~name attempt None;
    raise exn
  | attempt, _ ->
    let pause = exp_backoff_pause ~max_delay attempt in
    backoff_log ~exn ~name attempt (Some pause);
    wait_pause' pause;
    attempt + 1
