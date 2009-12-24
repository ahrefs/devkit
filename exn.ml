(**
  Dealing with exceptions
*)

let catch f x = try Some (f x) with _ -> None
let default def f x = try f x with _ -> def
let suppress f x = try f x with _ -> ()

let fail fmt = Printf.ksprintf failwith fmt

let set_printer, to_string = 
  let printer = ref Printexc.to_string in
  (fun f -> printer := f), (fun e -> !printer e)

let str = to_string

let log_s e s = Log.warn "%s : exception : %s" s (to_string e)
let log e fmt = Printf.ksprintf (log_s e) fmt

(** [log_try f x] logs and reraises any exception raised by [f x] *)
let log_try ?name f x = 
  try f x with e -> log_s e (Option.default "Exn.log_try" name); raise e
(** Apply [f x], exception (if any) is logged and suppressed. *)
let log_catch ?name f x =
  try f x with e -> log_s e (Option.default "Exn.log_catch" name)

let log_action ?name f x =
  try
    Option.may (Log.info "Action \"%s\" started") name;
    let t = Unix.gettimeofday () in
    let () = f x in
    Option.may (fun name -> Log.info "Action \"%s\" finished (%f secs)" name (Unix.gettimeofday () -. t)) name
  with
    e ->
      let name = Option.map_default (Printf.sprintf " \"%s\"") "" name in
      Log.error "Action%s aborted with uncaught exception : %s" name (str e);
      Log.error_s (Printexc.get_backtrace ())

let log_thread ?name f x =
  Thread.create (fun () -> log_action ?name f x) ()

