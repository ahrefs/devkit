(** Time *)

open Printf
open ExtLib

open Prelude

type t = float

let get = Unix.gettimeofday
let now = Unix.gettimeofday

let to_string ?(gmt=false) ?(ms=false) f =
  let t = (if gmt then Unix.gmtime else Unix.localtime) f in
  let sec = if ms then sprintf "%07.4f" (mod_float f 60.) else sprintf "%02u" t.Unix.tm_sec in
  sprintf "%04u-%02u-%02uT%02u:%02u:%s%s"
    (1900 + t.Unix.tm_year) (t.Unix.tm_mon+1) t.Unix.tm_mday t.Unix.tm_hour t.Unix.tm_min sec (if gmt then "Z" else "")

(** @see <http://www.w3.org/TR/NOTE-datetime> W3C Datetime *)
let gmt_string = to_string ~gmt:true ~ms:false
let gmt_string_ms = to_string ~gmt:true ~ms:true

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
  if t < 1. then sprintf "%.4f secs" t
  else if t < 10. then sprintf "%.2f secs" t
  else 
  loop (int_of_float t) [] factors >> List.combine names >> List.rev >> 
  List.dropwhile (fun (_,x) -> x = 0) >>
  List.map (fun (n,x) -> sprintf "%u %s" x n) >> String.concat " "

let minutes x = float & 60 * x
let hours x = minutes & 60 * x
let days x = hours & 24 * x

