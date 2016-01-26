(**
  Dealing with exceptions
*)

open Printf
open ExtLib

type 'a result = [ `Ok of 'a | `Exn of exn ]

let catch f x = try Some (f x) with _ -> None
let default def f x = try f x with _ -> def
let suppress f x = try f x with _ -> ()
let map f x = try `Ok (f x) with exn -> `Exn exn

let to_string = function
  | Unix.Unix_error (e,f,s) -> sprintf "Unix_error %s(%s) %s" f s (Unix.error_message e)
  | Xmlm.Error ((p1,p2),e) -> sprintf "Xmlm.Error((%u,%u),%s)" p1 p2 (Xmlm.error_message e)
  | Curl.CurlException (_,n,s) -> sprintf "Curl.CurlException(%u,%s)" n s
  | Pcre.Error err -> sprintf "Pcre.Error(%s)"
    begin match err with
    | Pcre.Partial -> "Partial"
    | Pcre.BadPartial -> "BadPartial"
    | Pcre.BadPattern(m,p) -> sprintf "BadPattern(%s,%i)" m p
    | Pcre.BadUTF8 -> "BadUTF8"
    | Pcre.BadUTF8Offset -> "BadUTF8Offset"
    | Pcre.MatchLimit -> "MatchLimit"
    | Pcre.RecursionLimit -> "RecursionLimit"
    | Pcre.InternalError s -> sprintf "InternalError(%s)" s
    end
  | exn -> Printexc.to_string exn

let str = to_string

let fail ?exn fmt =
  let fails s = match exn with None -> failwith s | Some exn -> failwith (s ^ " : " ^ to_string exn) in
  ksprintf fails fmt

let invalid_arg fmt = ksprintf invalid_arg fmt

let get_backtrace () = String.nsplit (Printexc.get_backtrace ()) "\n"
