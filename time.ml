(** Time *)

open Printf
open ExtLib

open Prelude

type t = float

let get = Unix.gettimeofday
let now = Unix.gettimeofday

(** @see <http://www.w3.org/TR/NOTE-datetime> W3C Datetime *)
let gmt_string t = 
  let module U = Unix in
  let t = U.gmtime t in
  sprintf "%04u-%02u-%02uT%02u:%02u:%02uZ" (1900 + t.U.tm_year) (t.U.tm_mon+1) t.U.tm_mday t.U.tm_hour t.U.tm_min t.U.tm_sec

let to_string t =
  let module U = Unix in
  let t = U.localtime t in
  sprintf "%04u-%02u-%02uT%02u:%02u:%02u" (1900 + t.U.tm_year) (t.U.tm_mon+1) t.U.tm_mday t.U.tm_hour t.U.tm_min t.U.tm_sec

let gmt_string_ms f = 
  let module U = Unix in
  let t = U.gmtime f in
  sprintf "%04u-%02u-%02uT%02u:%02u:%09fZ" (1900 + t.U.tm_year) (t.U.tm_mon+1) t.U.tm_mday t.U.tm_hour t.U.tm_min 
  (mod_float f 60.)

(** unix timestamp to RFC-2822 date
    Example: Tue, 15 Nov 1994 12:45:26 GMT *)
let to_rfc2822 secs =
  let module U = Unix in
  let t = U.gmtime secs in
  let wdays = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |] in
  let mons = [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|] in
  let wday = wdays.(t.U.tm_wday mod 7) in
  let mon = mons.(t.U.tm_mon mod 12) in
  sprintf "%s, %02u %s %04u %02u:%02u:%02u GMT" wday t.U.tm_mday mon (1900 + t.U.tm_year) t.U.tm_hour t.U.tm_min t.U.tm_sec

let duration_str t =
  let factors = [60; 60; 24; 30; 12;] in
  let names = ["secs"; "min"; "hours"; "days"; "months";] in
  let rec loop t acc = function
  | [] -> List.rev acc
  | n::tl -> loop (t/n) (t mod n :: acc) tl
  in
  match int_of_float t with
  | 0 -> sprintf "%.4f secs" t
  | n ->
  loop n [] factors >> List.combine names >> List.rev >> 
  List.dropwhile (fun (_,x) -> x = 0) >>
  List.map (fun (n,x) -> sprintf "%u %s" x n) >> String.concat " "

let minutes x = float & 60 * x
let hours x = minutes & 60 * x
let days x = hours & 24 * x

