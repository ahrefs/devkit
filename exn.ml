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

let to_string exn =
  match exn with
  | Unix.Unix_error (e,f,s) -> sprintf "Unix_error %s(%s) %s" f s (Unix.error_message e)
  | Curl.CurlException (_,n,s) -> sprintf "Curl.CurlException(%u,%s)" n s
  | Pcre.Error err -> sprintf "Pcre.Error(%s)"
    begin match err with
    | Partial -> "Partial"
    | BadPartial -> "BadPartial"
    | BadPattern(m,p) -> sprintf "BadPattern(%s,%i)" m p
    | BadUTF8 -> "BadUTF8"
    | BadUTF8Offset -> "BadUTF8Offset"
    | MatchLimit -> "MatchLimit"
    | RecursionLimit -> "RecursionLimit"
    | InternalError s -> sprintf "InternalError(%s)" s
    | _ -> Printexc.to_string exn
    end
  | exn -> Printexc.to_string exn

let str = to_string

let fail ?exn fmt =
  let fails s =
    match exn with
    | None -> failwith s
    | Some original_exn ->
      let orig_bt = Printexc.get_raw_backtrace () in
      let exn = Failure (s ^ " : " ^ to_string original_exn) in
      Printexc.raise_with_backtrace exn orig_bt
  in
  ksprintf fails fmt

let invalid_arg fmt = ksprintf invalid_arg fmt

let get_backtrace () = String.nsplit (Printexc.get_backtrace ()) "\n"
