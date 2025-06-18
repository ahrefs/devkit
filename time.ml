(** Time *)

open Printf
open ExtLib

open Prelude

(** unix timestamp *)
type t = float

(* duration in seconds *)
type duration = float

include (Devkit_ragel : sig exception Parse_compact_duration of string end)

let compare = Factor.Float.compare

let get = Unix.gettimeofday
let now = Unix.gettimeofday

let unsafe_digit n = Char.unsafe_chr (Char.code '0' + n)
let put_2d s ofs n =
  Bytes.unsafe_set s ofs (unsafe_digit (n / 10));
  Bytes.unsafe_set s (ofs+1) (unsafe_digit (n mod 10))

let yyyy = 2025

let replace_yyyy s t =
  let year = t.Unix.tm_year + 1900 in
  if year <> yyyy then
  begin
    if year >= yyyy && year < 2030 then
      Bytes.unsafe_set s 3 (unsafe_digit (year mod 10))
    else
      Bytes.unsafe_blit (Bytes.unsafe_of_string @@ if year >= 1000 then string_of_int year else sprintf "%04u" year) 0 s 0 4
  end

let yyyy = string_of_int yyyy

let fast_to_string =
  (* "%04u-%02u-%02uT%02u:%02u:%02u%s" *)
  let template = yyyy ^ "-__-__T__:__:__" in
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
    replace_yyyy s t;
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
  let template = yyyy^"-__-__" in
  fun t ->
    let open Unix in
    let s = Bytes.of_string template in
    replace_yyyy s t;
    put_2d s 5 (t.tm_mon+1);
    put_2d s 8 t.tm_mday;
    Bytes.unsafe_to_string s

(** YYYYMMDD *)
let format_date8 =
  let template = yyyy^"____" in
  fun t ->
    let open Unix in
    let s = Bytes.of_string template in
    replace_yyyy s t;
    put_2d s 4 (t.tm_mon+1);
    put_2d s 6 t.tm_mday;
    Bytes.unsafe_to_string s

(** YYYYMMDDhh *)
let format_date8h =
  let template = yyyy^"______" in
  fun t ->
    let open Unix in
    let s = Bytes.of_string template in
    replace_yyyy s t;
    put_2d s 4 (t.tm_mon+1);
    put_2d s 6 t.tm_mday;
    put_2d s 8 t.tm_hour;
    Bytes.unsafe_to_string s

(** YYYYMMDDhhmm *)
let format_date8hm =
  let template = yyyy^"________" in
  fun t ->
    let open Unix in
    let s = Bytes.of_string template in
    replace_yyyy s t;
    put_2d s 4 (t.tm_mon+1);
    put_2d s 6 t.tm_mday;
    put_2d s 8 t.tm_hour;
    put_2d s 10 t.tm_min;
    Bytes.unsafe_to_string s

(** YYYYMMDDhhmmss *)
let format_date8hms =
  let template = yyyy^"__________" in
  fun t ->
    let open Unix in
    let s = Bytes.of_string template in
    replace_yyyy s t;
    put_2d s 4 (t.tm_mon+1);
    put_2d s 6 t.tm_mday;
    put_2d s 8 t.tm_hour;
    put_2d s 10 t.tm_min;
    put_2d s 12 t.tm_sec;
    Bytes.unsafe_to_string s

(** YYYY-MM-DD hh:mm:ss *)
let format_basic =
  let template = yyyy^"-__-__ __:__:__" in
  fun t ->
    let open Unix in
    let s = Bytes.of_string template in
    replace_yyyy s t;
    put_2d s 5 (t.tm_mon+1);
    put_2d s 8 t.tm_mday;
    put_2d s 11 t.tm_hour;
    put_2d s 14 t.tm_min;
    put_2d s 17 t.tm_sec;
    Bytes.unsafe_to_string s

(** MMDD *)
let format_date4 t =
  let open Unix in
  let s = Bytes.create 4 in
  put_2d s 0 (t.tm_mon+1);
  put_2d s 2 t.tm_mday;
  Bytes.unsafe_to_string s

(** YYYYMM *)
let format_date_yyyymm =
  let template = yyyy^"__" in
  fun t ->
    let s = Bytes.of_string template in
    replace_yyyy s t;
    put_2d s 4 (t.tm_mon+1);
    Bytes.unsafe_to_string s

let date_w3_gmt_string = format_date_w3 $ Unix.gmtime
let date_w3_string = format_date_w3 $ Unix.localtime

let date8_gmt_string = format_date8 $ Unix.gmtime
let date8_string = format_date8 $ Unix.localtime

let date8h_gmt_string = format_date8h $ Unix.gmtime
let date8h_string = format_date8h $ Unix.localtime

let date8hm_gmt_string = format_date8hm $ Unix.gmtime
let date8hm_string = format_date8hm $ Unix.localtime

let date8hms_gmt_string = format_date8hms $ Unix.gmtime
let date8hms_string = format_date8hms $ Unix.localtime

let basic_gmt_string = format_basic $ Unix.gmtime
let basic_string = format_basic $ Unix.localtime

let date4_gmt_string = format_date4 $ Unix.gmtime
let date4_string = format_date4 $ Unix.localtime

let date_gmt_yyyymm_string = format_date_yyyymm $ Unix.gmtime
let date_yyyymm_string = format_date_yyyymm $ Unix.localtime

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
let show_duration ?cut t =
  let factors = [60; 60; 24; 365 ] in
  let names = ["second"; "minute"; "hour"; "day"; "year";] in
  let rec loop t acc = function
  | [] -> List.rev (t :: acc)
  | n::tl -> loop (t/n) (t mod n :: acc) tl
  in
  if t < 1. then sprintf "%.4f seconds" t
  else if t < 10. then sprintf "%.2f seconds" t
  else
  loop (int_of_float t) [] factors |> List.combine names |> List.rev |>
  List.dropwhile (fun (_,x) -> x = 0) |>
  (match cut with Some n -> List.take n | None -> id) |>
  List.filter (fun (_,x) -> x <> 0) |>
  List.map (fun (n,x) -> sprintf "%u %s%s" x n (if x <> 1 then "s" else "")) |> String.concat " "

let duration_str = show_duration

(* 1m10s *)
let show_compact_duration ?(full=false) ?cut t =
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
  loop (int_of_float t) [] factors |> List.combine names |> List.rev |>
  List.dropwhile (fun (_,x) -> x = 0) |>
  (match cut with Some n -> List.take n | None -> id) |>
  (if full then id else List.filter (fun (_,x) -> x <> 0)) |>
  List.mapi (fun i (n,x) -> sprintf (if i = 0 then "%u%s" else "%02u%s") x n) |> String.concat ""

let compact_duration = show_compact_duration

(** parse compact_duration representation (except for fractional seconds) *)
let of_compact_duration s =
  match s with
  | "" -> invalid_arg "of_compact_duration empty input"
  | _ ->
    let s = match s.[0] with
    | '0'..'9' -> s
    | _ -> "1"^s
  in
  Devkit_ragel.parse_compact_duration s

let seconds_1m = 60
let seconds_1h = seconds_1m * 60
let seconds_1d = seconds_1h * 24
let seconds_1y = seconds_1d * 365

let time_x factor = (fun x -> float @@ factor * x), (fun x -> int_of_float x / factor), (fun x -> float (int_of_float x / factor * factor))
let (seconds,to_seconds,round_seconds) = time_x 1
let (minutes,to_minutes,round_minutes) = time_x seconds_1m
let (hours,to_hours,round_hours) = time_x seconds_1h
let (days,to_days,round_days) = time_x seconds_1d
let (years,to_years,round_years) = time_x seconds_1y

(** convert integer number of milliseconds to Time.t *)
let msec x = float x /. 1000.

(** convert integer number of nanoseconds to Time.t *)
let nsec x = float x /. 1_000_000_000.

let to_ms x = int_of_float @@ 1000. *. x

let ago t = now () -. t
let ago_str = show_duration $ ago

(* compat *)
let int = to_seconds
let to_sec = to_seconds
