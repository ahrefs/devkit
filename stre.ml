(** More string functions *)

open Prelude
open ExtLib

let by_words = Pcre.regexp ~flags:[`UTF8] "(?:[^\\pL\\pN.])+"
let by_space = Pcre.regexp "\\s+"
let by_lines = Pcre.regexp "\\r?\\n"
let split rex str = match Pcre.split ~rex str with ""::l -> l | l -> l

let nsplitc_rev str sep =
	if str = "" then []
	else
		let rec nsplit acc p =
      match try Some (String.index_from str p sep) with Not_found -> None with
      | Some p2 -> nsplit (String.sub str p (p2 - p) :: acc) (p2 + 1)
      | None -> String.sub str p (String.length str - p) :: acc
		in
		nsplit [] 0

let nsplitc str sep = List.rev (nsplitc_rev str sep)

let countc s c = String.fold_left (fun acc c' -> if c = c' then acc+1 else acc) 0 s

(** remove prefix from string if present *)
let drop_prefix s pre = if String.starts_with s pre then String.slice s ~first:(String.length pre) else s
let drop_suffix s suf = if String.ends_with s suf then String.slice ~last:(String.length s - String.length suf) s else s

(*
let replace_all ~str ~sub ~by =
  Str.global_substitute (Str.regexp_string sub) (fun _ -> by) str
*)

let qreplace str sub by =
  Pcre.qreplace ~rex:(Pcre.regexp & Pcre.quote sub) ~templ:by str

(** contents of the first submatch *)
let extract rex str = 
  try
    Some (Pcre.extract ~rex ~full_match:false str).(0)
  with
    _ -> None

let istarts_with s ?(pos=0) prefix =
  if pos < 0 || pos > String.length s then invalid_arg "Stre.istarts_with";
  if String.length s < pos + String.length prefix then false else
  try
  for i = 0 to String.length prefix - 1 do
    let c1 = s.[pos + i] and c2 = prefix.[i] in
    if c1 <> c2 && Char.lowercase c1 <> Char.lowercase c2 then raise Not_found
  done; true
  with Not_found -> false

(** ascii case-insensitive equality *)
let iequal s1 s2 = String.length s1 = String.length s2 && istarts_with s1 s2

let equal (s1:string) s2 = s1 = s2

(** ascii case-insensitive substring *)
let iexists s sub =
  if String.length s < String.length sub then false else
  if sub = "" then true else
  try
  for i = 0 to String.length s - String.length sub do
    if istarts_with s ~pos:i sub then raise Exit;
  done; false
  with Exit -> true

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

module ASCII = struct
let is_alpha = function
| 'a'..'z' | 'A'..'Z' -> true
| _ -> false
let is_digit = function
| '0'..'9' -> true
| _ -> false
let is_alnum c = is_alpha c || is_digit c
end

let unescaped s = 
  try Scanf.sscanf ("\""^s^"\"") "%S%!" (fun a -> a)  with  _ -> (Exn.fail "Stre.unescaped : Wry input %s" s)

let revert = String.implode $ List.rev $ String.explode

let find_prefix s1 s2 =
  let i = ref 0 in
  let min_len = min (String.length s1) (String.length s2) in
  while !i < min_len && s1.[!i] = s2.[!i] do incr i done;
  !i
