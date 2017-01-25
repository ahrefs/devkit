(**
  Dealing with Lwt exceptions
*)

open Printf

let catch f x = Lwt.try_bind (fun () -> f x) Lwt.return_some (fun _exn -> Lwt.return_none)
let map f x = Lwt.try_bind (fun () -> f x) (fun r -> Lwt.return (`Ok r)) (fun exn -> Lwt.return (`Exn exn))

let fail ?exn fmt =
  let fails s = Lwt.fail_with @@ match exn with None -> s | Some exn -> s ^ " : " ^ Exn.to_string exn in
  ksprintf fails fmt

let invalid_arg fmt = ksprintf Lwt.fail_invalid_arg fmt
