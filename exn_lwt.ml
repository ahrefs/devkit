open Printf

let catch f x = Lwt.catch (fun () -> Lwt.bind (f x) (fun r -> Lwt.return (Some r))) (fun _ -> Lwt.return None)
let map f x = Lwt.catch (fun () -> Lwt.bind (f x) (fun r -> Lwt.return (`Ok r))) (fun exn -> Lwt.return (`Exn exn))

let fail ?exn fmt =
  let fails s = match exn with None -> Lwt.fail (Failure s) | Some exn -> Lwt.fail (Failure (s ^ " : " ^ Exn.to_string exn)) in
  ksprintf fails fmt

let invalid_arg fmt = ksprintf (fun s -> Lwt.fail (Invalid_argument s)) fmt

let exn_of_result = function
  | `Error msg -> Lwt.fail_with msg
  | `Ok v -> Lwt.return v
