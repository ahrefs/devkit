(** More string functions *)

open Prelude
open ExtLib

let by_words = Pcre.regexp ~flags:[`UTF8] "(?:[^\\pL\\pN.]|_)+"
let split rex str = match Pcre.split ~rex str with ""::l -> l | l -> l

(*
let replace_all ~str ~sub ~by =
  Str.global_substitute (Str.regexp_string sub) (fun _ -> by) str
*)

(** contents of the first submatch *)
let extract rex str = 
  try
    Some (Pcre.extract ~rex ~full_match:false str).(0)
  with
    _ -> None

(** ascii case-insensitive equality *)
let iequal s1 s2 =
  if String.length s1 <> String.length s2 then false else
  try
  for i = 0 to String.length s1 - 1 do
    let c1 = s1.[i] and c2 = s2.[i] in
    if c1 <> c2 && Char.lowercase c1 <> Char.lowercase c2 then raise Not_found
  done; true
  with Not_found -> false

(** sequence of matches *)
let enum_matches rex s =
  try
    Pcre.exec_all ~rex s >> Array.enum
  with
    _ -> Enum.empty () 

let enum_extract rex s = enum_matches rex s >> Enum.map (flip Pcre.get_substring 1)

let erase_dots s = 
  let rec erase = parser
  | [< ''\''; t >] -> erase t
  | [< ''0'..'9' as d; t >] -> after_digit d t
  | [< ''.'; t=dots >] -> [< '' '; erase t >]
  | [< 'c; t >] -> [< 'c; erase t >]
  | [< >] -> [< >]
  and after_digit d1 = parser 
  | [< ''.'; t >] -> 
    begin match Stream.peek t with
    | Some ('0'..'9') -> [< 'd1; ''.'; erase t >]
    | _ -> [< 'd1; '' '; erase t >]
    end
  | [< t >] -> [< 'd1; erase t >]
  and dots = parser
  | [< ''.'; t >] -> dots t
  | [< t >] -> t
  in
  erase (Stream.of_string s)

let string_of_stream s =
  let b = Buffer.create 32 in Stream.iter (Buffer.add_char b) s; Buffer.contents b

let split_words = split by_words $ string_of_stream $ erase_dots
let split_words s = try split_words s with exn -> Log.self #warn ~exn "split_words \"%s\"" s; [s]

let concat sep e = String.concat " " (List.of_enum e)

