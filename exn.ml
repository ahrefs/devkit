(**
  Dealing with exceptions
*)

let catch f x = try Some (f x) with _ -> None
let suppress f x = try f x with _ -> ()

let fail fmt = Printf.ksprintf failwith fmt

let set_printer, to_string = 
  let printer = ref Printexc.to_string in
  (fun f -> printer := f), (fun e -> !printer e)

let str = to_string

let log_s e s = Log.warn "%s : exception : %s" s (to_string e)
let log e fmt = Printf.ksprintf (log_s e) fmt

(** [log_try f x] logs and reraises any exception raised by [f x] *)
let log_try f x = try f x with e -> log e "Exn.log_try"; raise e

