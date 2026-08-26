open Devkit

let fail expected actual =
  Printf.eprintf "expected:\n%s\nactual:\n%s\n" expected actual;
  exit 1

let expect_invalid_rate rate =
  match Control.Rate_limit.create ~allowed_per_sec:rate () with
  | exception Invalid_argument _ -> ()
  | _ -> fail "Invalid_argument" "rate limiter created"

let expect_invalid_capacity burst_capacity =
  match Control.Rate_limit.create ~burst_capacity ~allowed_per_sec:1. () with
  | exception Invalid_argument _ -> ()
  | _ -> fail "Invalid_argument" "rate limiter created"

let logging_lines count =
  let rec loop i acc =
    if i < 0 then acc else loop (i - 1) (Printf.sprintf "logging %d" i :: acc)
  in
  loop (count - 1) []

let () =
  List.iter expect_invalid_rate [0.; -1.; infinity; nan];
  List.iter expect_invalid_capacity [0; -1];

  (* A very low rate must still have capacity for its initial token. *)
  let slow = Control.Rate_limit.create ~allowed_per_sec:0.01 () in
  if not (Control.Rate_limit.attempt slow) then fail "allowed" "rate limited";
  if Control.Rate_limit.attempt slow then fail "rate limited" "allowed";
  if Control.Rate_limit.take_rate_limited_count slow <> 1 then
    fail "one rate-limited attempt" "unexpected count";
  if Control.Rate_limit.take_rate_limited_count slow <> 0 then
    fail "reset rate-limited count" "non-zero count";

  let output = Buffer.create 256 in
  let target = { Logger.
    format = (fun _level _facility _timestamp _pairs message -> message);
    output = (fun _level _facility message ->
      Buffer.add_string output message;
      Buffer.add_char output '\n');
  } in
  let logger = Logger.put_simple target in
  let log = new Log.logger ~logger (Log.facility "rate-limit-test") in
  let rate_limit = Control.Rate_limit.create ~burst_capacity:7 ~allowed_per_sec:2. () in
  let emit count =
    for i = 0 to count - 1 do
      log#info ~rate_limit "logging %d" i
    done
  in
  emit 10_000;
  Unix.sleep 2;
  (* Emit only the number guaranteed to have been refilled. This keeps a
     delayed test process from changing the expected output. *)
  emit 4;
  let expected =
    String.concat "\n"
      (logging_lines 7 @
       ["(9993 messages have been rate limited)"] @
       logging_lines 4 @ [""])
  in
  let actual = Buffer.contents output in
  if actual <> expected then fail expected actual
