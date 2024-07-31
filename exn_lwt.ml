(**
  Dealing with Lwt exceptions
*)

open Printf

let catch f x = Lwt.try_bind (fun () -> f x) Lwt.return_some (fun _exn -> Lwt.return_none)
let map f x = Lwt.try_bind (fun () -> f x) (fun r -> Lwt.return (`Ok r)) (fun exn -> Lwt.return (`Exn exn))

let fail = Exn.fail

let invalid_arg fmt = ksprintf Lwt.fail_invalid_arg fmt
