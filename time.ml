(** Time *)

open Printf
open ExtLib

open Prelude

(** unix timestamp or duration in seconds *)
type t = float

let get = Unix.gettimeofday
let now = Unix.gettimeofday

let unsafe_digit n = Char.unsafe_chr (Char.code '0' + n)
let put_2d s ofs n =
  Bytes.unsafe_set s ofs (unsafe_digit (n / 10));
  Bytes.unsafe_set s (ofs+1) (unsafe_digit (n mod 10))
let replace_year_2015 s year =
  if year <> 2015 then
  begin
    if year >= 2010 && year < 2020 then
      Bytes.unsafe_set s 3 (unsafe_digit (year mod 10))
    else
      Bytes.unsafe_blit (Bytes.unsafe_of_string @@ if year >= 1000 then string_of_int year else sprintf "%04u" year) 0 s 0 4
  end

let fast_to_string =
  (* "%04u-%02u-%02uT%02u:%02u:%02u%s" *)
  let template = "2015-__-__T__:__:__" in
  let template_z = template ^ "Z" in
  let last_time = ref 0 in
  let last_gmt = ref true in
  let last_s = ref "1970-01-01T00:00:00Z" in
  fun ~gmt f ->
    let seconds = int_of_float f in
    if gmt = !last_gmt && seconds = !last_time then !last_s
    else
    let open Unix in
    let t = (if gmt then gmtime else localtime) f in
    let s = Bytes.of_string (if gmt then template_z else template) in
    replace_year_2015 s (t.tm_year + 1900);
    put_2d s 5 (t.tm_mon+1);
    put_2d s 8 t.tm_mday;
    put_2d s 11 t.tm_hour;
    put_2d s 14 t.tm_min;
    put_2d s 17 t.tm_sec;
    last_time := seconds;
    last_gmt := gmt;
    let s = Bytes.unsafe_to_string s in
    last_s := s;
    s

let to_string ?(gmt=false) ?(ms=false) f =
  match ms with
  | false -> fast_to_string ~gmt f
  | true ->
    let t = (if gmt then Unix.gmtime else Unix.localtime) f in
    let sec = sprintf "%07.4f" (mod_float f 60.) in
    sprintf "%04u-%02u-%02uT%02u:%02u:%s%s"
      (1900 + t.Unix.tm_year) (t.Unix.tm_mon+1) t.Unix.tm_mday t.Unix.tm_hour t.Unix.tm_min sec (if gmt then "Z" else "")

(** @see <http://www.w3.org/TR/NOTE-datetime> W3C Datetime *)
let gmt_string = to_string ~gmt:true ~ms:false
let gmt_string_ms = to_string ~gmt:true ~ms:true

(** YYYY-MM-DD *)
let format_date_w3 =
  let template = "2015-__-__" in
  fun t ->
    let open Unix in
    let s = Bytes.of_string template in
    replace_year_2015 s (t.tm_year + 1900);
    put_2d s 5 (t.tm_mon+1);
    put_2d s 8 t.tm_mday;
    Bytes.unsafe_to_string s

(** YYYYMMDD *)
let format_date8 =
  let template = "2015____" in
  fun t ->
    let open Unix in
    let s = Bytes.of_string template in
    replace_year_2015 s (t.tm_year + 1900);
    put_2d s 4 (t.tm_mon+1);
    put_2d s 6 t.tm_mday;
    Bytes.unsafe_to_string s

(** YYYYMMDDhh *)
let format_date8h =
  let template = "2015______" in
  fun t ->
    let open Unix in
    let s = Bytes.of_string template in
    replace_year_2015 s (t.tm_year + 1900);
    put_2d s 4 (t.tm_mon+1);
    put_2d s 6 t.tm_mday;
    put_2d s 8 t.tm_hour;
    Bytes.unsafe_to_string s

(** YYYYMMDDhhmm *)
let format_date8hm =
  let template = "2015________" in
  fun t ->
    let open Unix in
    let s = Bytes.of_string template in
    replace_year_2015 s (t.tm_year + 1900);
    put_2d s 4 (t.tm_mon+1);
    put_2d s 6 t.tm_mday;
    put_2d s 8 t.tm_hour;
    put_2d s 10 t.tm_min;
    Bytes.unsafe_to_string s

(** YYYYMMDDhhmmss *)
let format_date8hms =
  let template = "2015__________" in
  fun t ->
    let open Unix in
    let s = Bytes.of_string template in
    replace_year_2015 s (t.tm_year + 1900);
    put_2d s 4 (t.tm_mon+1);
    put_2d s 6 t.tm_mday;
    put_2d s 8 t.tm_hour;
    put_2d s 10 t.tm_min;
    put_2d s 12 t.tm_sec;
    Bytes.unsafe_to_string s

(** MMDD *)
let format_date4 t =
  let open Unix in
  let s = Bytes.create 4 in
  put_2d s 0 (t.tm_mon+1);
  put_2d s 2 t.tm_mday;
  Bytes.unsafe_to_string s

let date_w3_gmt_string = format_date_w3 $ Unix.gmtime
let date_w3_string = format_date_w3 $ Unix.localtime

let date8_gmt_string = format_date8 $ Unix.gmtime
let date8_string = format_date8 $ Unix.localtime

let date8hm_gmt_string = format_date8hm $ Unix.gmtime
let date8hm_string = format_date8hm $ Unix.localtime

let date8h_gmt_string = format_date8h $ Unix.gmtime
let date8h_string = format_date8h $ Unix.localtime

let date4_gmt_string = format_date4 $ Unix.gmtime
let date4_string = format_date4 $ Unix.localtime

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

(** @param cut - only show this number of most significant components *)
let duration_str ?cut t =
  let factors = [60; 60; 24; 30; 12;] in
  let names = ["secs"; "min"; "hours"; "days"; "months";] in
  let rec loop t acc = function
  | [] -> List.rev acc
  | n::tl -> loop (t/n) (t mod n :: acc) tl
  in
  if t < 1. then sprintf "%.4f secs" t
  else if t < 10. then sprintf "%.2f secs" t
  else
  loop (int_of_float t) [] factors |> List.combine names |> List.rev |>
  List.dropwhile (fun (_,x) -> x = 0) |>
  (match cut with Some n -> List.take n | None -> id) |>
  List.map (fun (n,x) -> sprintf "%u %s" x n) |> String.concat " "

(* 1m10s *)
let compact_duration ?(full=false) ?cut t =
  let factors = [60; 60; 24; ] in
  let names = ["s"; "m"; "h"; "d"; ] in
  let rec loop t acc = function
  | [] -> List.rev (t::acc)
  | n::tl -> loop (t/n) (t mod n :: acc) tl
  in
  if t < 0.000_01 then sprintf "%.0fns" (t *. 1_000_000_000.)
  else if t < 0.01 then sprintf "%.2gms" (t *. 1_000.)
  else if t < 1. then sprintf "%.0fms" (t *. 1_000.)
  else if t < 10. then sprintf "%.2gs" t
  else
  loop (int_of_float t) [] factors |> List.combine names |>
  (if full then id else List.dropwhile (fun (_,x) -> x = 0)) |>
  List.rev |>
  List.dropwhile (fun (_,x) -> x = 0) |>
  (match cut with Some n -> List.take n | None -> id) |>
  List.mapi (fun i (n,x) -> sprintf (if i = 0 then "%u%s" else "%02u%s") x n) |> String.concat ""

(** parse compact_duration representation (except for fractional seconds) *)
let of_compact_duration s = Devkit_ragel.parse_compact_duration s
(* TODO ms ns not parsed *)

let minutes x = float @@ 60 * x
let hours x = minutes @@ 60 * x
let days x = hours @@ 24 * x
let seconds x = float x

(** convert integer number of milliseconds to Time.t *)
let msec x = float x /. 1000.

(** convert integer number of nanoseconds to Time.t *)
let nsec x = float x /. 1_000_000_000.

let int x = int_of_float x
let to_sec = int
let to_ms x = int_of_float @@ 1000. *. x

let ago t = now () -. t
let ago_str = duration_str $ ago
