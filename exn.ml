(**
  Dealing with exceptions
*)

let catch f x = try Some (f x) with _ -> None
let default def f x = try f x with _ -> def
let suppress f x = try f x with _ -> ()
let map f x = try `Ok (f x) with exn -> `Exn exn

let fail fmt = Printf.ksprintf failwith fmt

let set_printer, to_string = 
  let printer = ref Printexc.to_string in
  (fun f -> printer := f), (fun e -> !printer e)

let str = to_string

(*
(** [log_try f x] logs and reraises any exception raised by [f x] *)
let log_try ?name f x = 
  try f x with e -> log_s e (Option.default "Exn.log_try" name); raise e
(** Apply [f x], exception (if any) is logged and suppressed. *)
let log_catch ?name f x =
  try f x with e -> log_s e (Option.default "Exn.log_catch" name)
*)

