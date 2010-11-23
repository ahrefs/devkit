(**
  Dealing with exceptions
*)

open Printf

let catch f x = try Some (f x) with _ -> None
let default def f x = try f x with _ -> def
let suppress f x = try f x with _ -> ()
let map f x = try `Ok (f x) with exn -> `Exn exn

let fail fmt = ksprintf failwith fmt

(*
let set_printer, to_string = 
  let printer = ref Printexc.to_string in
  (fun f -> printer := f), (fun e -> !printer e)
*)

let to_string = function
  | Unix.Unix_error (e,f,s) -> sprintf "Unix_error %s(%s) %s" f s (Unix.error_message e)
  | Xmlm.Error ((p1,p2),e) -> sprintf "Xmlm.Error((%u,%u),%s)" p1 p2 (Xmlm.error_message e)
  | Curl.CurlException (_,n,s) -> sprintf "Curl.CurlException(%u,%s)" n s
  | exn -> Printexc.to_string exn

let str = to_string

(*
(** [log_try f x] logs and reraises any exception raised by [f x] *)
let log_try ?name f x = 
  try f x with e -> log_s e (Option.default "Exn.log_try" name); raise e
(** Apply [f x], exception (if any) is logged and suppressed. *)
let log_catch ?name f x =
  try f x with e -> log_s e (Option.default "Exn.log_catch" name)
*)

