open Devkit

let fail expected actual =
  Printf.eprintf "expected:\n%s\nactual:\n%s\n" expected actual;
  exit 1

let () =
  let output = Buffer.create 256 in
  let target = { Logger.
    format = (fun _level _facility _timestamp _pairs message -> message);
    output = (fun _level _facility message ->
      Buffer.add_string output message;
      Buffer.add_char output '\n');
  } in
  let logger = Logger.put_simple target in
  let log = new Log.logger ~logger (Log.facility "rate-limit-test") in
  let rate_limit = Log.Rate_limit.create ~max:2 ~period:1. () in
  let emit () =
    for i = 0 to 9_999 do
      log#info ~rate_limit "logging %d" i
    done
  in
  emit ();
  Unix.sleep 2;
  emit ();
  let expected =
    String.concat "\n" [
      "logging 0";
      "logging 1";
      "(9998 messages have been rate limited)";
      "logging 0";
      "logging 1";
      "";
    ]
  in
  let actual = Buffer.contents output in
  if actual <> expected then fail expected actual
