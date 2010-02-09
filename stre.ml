(** More string functions *)

open Prelude
open ExtLib

let by_words = Pcre.regexp ~flags:[`UTF8] "(?:[^\\pL\\pN.]|_)+"
let split rex str = match Pcre.split ~rex str with ""::l -> l | l -> l

let extract rex str = 
  try
    Some (Pcre.extract ~rex ~full_match:false str).(0)
  with
    _ -> None

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
let split_words s = try split_words s with e -> Exn.log e "split_words \"%s\"" s; [s]

let concat sep e = String.concat " " (List.of_enum e)

