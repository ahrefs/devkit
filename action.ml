(** Misc *)

open ExtLib
open Printf

open Prelude

let period n f = 
  let count = ref 0 in
  (fun () -> incr count; if !count mod n = 0 then f !count)

let strl f l = sprintf "[%s]" (String.concat ";" (List.map f l))

let uniq p e =
  let h = Hashtbl.create 16 in
  Enum.filter (fun x ->
    let k = p x in
    if Hashtbl.mem h k then false else (Hashtbl.add h k (); true)) e

let list_uniq p = List.of_enum $ uniq p $ List.enum

let list_random_exn l = List.nth l (Random.int (List.length l))

let list_random = function
  | [] -> None
  | l -> list_random_exn l >> some

(** [partition l n] splits [l] into [n] chunks *)
let partition l n =
  if n < 2 then [| l |] else
  let a = Array.make n [] in
  ExtList.List.iteri (fun i x -> let i = i mod n in a.(i) <- x :: a.(i)) l;
  a

let file_lines_exn file =
  let ch = open_in file in
  let l = Std.input_lines ch >> List.of_enum in
  close_in_noerr ch;
  l

let file_lines file = try file_lines_exn file with _ -> []

(** array must be sorted *)
let binary_search arr cmp x =
  let rec loop a b =
    match b - a with
    | 0 -> false
    | 1 -> cmp arr.(a) x = 0
    | n ->
      let mid = a + n / 2 in
      let v = arr.(mid) in
      match cmp v x with
      | 0 -> true 
      | 1 -> loop a mid
      | _ (* -1 *) -> loop (mid+1) b
  in
  loop 0 (Array.length arr)

(** [chunks e n] splits [e] into chunks of [n] elements each (except the last which can be shorter) *)
(*
let chunks e n =
  let rec loop len =
    if len = n 
    match Enum.
  ExtList.List.iteri (fun i x -> let i = i mod n in a.(i) <- x :: a.(i)) l;
  a
*)

(* FIXME *)

let bytes_string_f f = (* oh ugly *)
  if f < 1024. then sprintf "%uB" (int_of_float f) else
  if f < 1024. *. 1024. then sprintf "%uKB" (int_of_float (f /. 1024.)) else
  if f < 1024. *. 1024. *. 1024. then sprintf "%.1fMB" (f /. 1024. /. 1024.) else
  sprintf "%.1fGB" (f /. 1024. /. 1024. /. 1024.)

let bytes_string = bytes_string_f $ float_of_int

let caml_words_f f =
  bytes_string_f (f *. (float_of_int (Sys.word_size / 8)))

let caml_words = caml_words_f $ float_of_int

(* EMXIF *)

module App(Info : sig val version : string val name : string end) = struct

let run main =
  Printexc.record_backtrace true;
  Log.self #info "%s started. Version %s. PID %u" Info.name Info.version (Unix.getpid ());
  try
    main ();
    Log.self #info "%s finished." Info.name
  with
    e -> Log.self #error "%s aborted : %s" Info.name (Exn.str e); Log.self #error "%s" (Printexc.get_backtrace ())

end

class timer = 
let tm = Unix.gettimeofday  in
object

val mutable start = tm ()
method reset = start <- tm ()
method get = tm () -. start
method gets = sprintf "%.6f" & tm () -. start
method get_str = Time.duration_str & tm () -. start

end

let log ?name f x =
  try
    Option.may (Log.self #info "Action \"%s\" started") name;
    let t = Unix.gettimeofday () in
    let () = f x in
    Option.may (fun name -> Log.self #info "Action \"%s\" finished (%f secs)" name (Unix.gettimeofday () -. t)) name
  with
    e ->
      let name = Option.map_default (Printf.sprintf " \"%s\"") "" name in
      Log.self #error "Action%s aborted with uncaught exception : %s" name (Exn.str e);
      Log.self #error "%s" (Printexc.get_backtrace ())

let log_thread ?name f x =
  Thread.create (fun () -> log ?name f x) ()

(** Copy all data from [input] to [output] *)
let io_copy input output =
  try
    let size = 16 * 1024 in
    let s = String.create size in
    while true do
      let n = IO.input input s 0 size in
      if n = 0 then raise IO.No_more_input;
      ignore & IO.really_output output s 0 n
    done
  with IO.No_more_input -> ()

let io_null = IO.create_out (fun _ -> ()) (fun _ _ len -> len) id id

let compare_by f a b = compare (f a) (f b)

let hexdump str =
  let buf = Buffer.create 80 and num = ref 0 in
  let rec loop chars =
    match List.take 16 chars with
    | [] -> Buffer.contents buf
    | l ->
          bprintf buf "%08x|  " !num;
          num := !num + 16;
          let rec bytes pos = function
            | [] -> 
                blanks pos
            | x :: l ->
                if pos = 8 then Buffer.add_char buf ' ';
                Printf.bprintf buf "%02x " (Char.code x);
                bytes (pos + 1) l
          and blanks pos =
            if pos < 16 then begin
              if pos = 8 then
                Buffer.add_string buf "    "
              else
                Buffer.add_string buf "   ";
              blanks (pos + 1)
            end
          in
          bytes 0 l;
          Buffer.add_string buf " |";
          List.iter (fun ch -> Buffer.add_char buf (if ch >= '\x20' && ch <= '\x7e' then ch else '.')) l;
          Buffer.add_char buf '|';
          Buffer.add_char buf '\n';
          loop (List.drop 16 chars)
  in
   loop (String.explode str)

