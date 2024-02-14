(** More string functions *)

open Printf
open ExtLib
open Prelude

let by_words = Pcre.regexp ~flags:[`UTF8] "[^\\pL\\pN.]+"
let by_space = Pcre.regexp "\\s+"
let by_lines = Pcre.regexp "\\r?\\n"
let split rex str = match Pcre.split ~rex str with ""::l -> l | l -> l

let nsplitc_enum str sep =
  let p = ref 0 in
  let n = String.length str in
  let next () =
    if !p >= n then raise Enum.No_more_elements;
    match String.index_from str !p sep with
    | p2 -> let s = String.sub str !p (p2 - !p) in p := p2 + 1; s
    | exception Not_found -> let s = String.sub str !p (n - !p) in p := n; s
  in
  Enum.from next

let nsplitc_fold str sep fold zero =
  if str = "" then zero
  else
    let rec nsplit acc p =
      match String.index_from str p sep with
      | p2 -> nsplit (fold acc (String.sub str p (p2 - p))) (p2 + 1)
      | exception Not_found -> fold acc (String.sub str p (String.length str - p))
    in
    nsplit zero 0

let nsplitc_rev str sep = nsplitc_fold str sep cons []

let nsplitc str sep = List.rev (nsplitc_rev str sep)

let countc s c = String.fold_left (fun acc c' -> if c = c' then acc+1 else acc) 0 s

(** [from_to s a b]
  @return substring of [s] from position [a] (including) up to position [b] (excluding).
  @raise Invalid_argument if [a] is greater than [b] or any position is out of bounds *)
let from_to s a b = String.sub s a (b - a)

(** [from_to] without safety checks *)
let unsafe_from_to s a b =
  let r = Bytes.create (b - a) in
  String.unsafe_blit s a r 0 (b - a);
  Bytes.unsafe_to_string r

(** [upto s p] @return prefix of [s] up to position [p] (excluding) *)
let upto s p = String.sub s 0 p

(** [from s p] @return suffix of [s] from position [p] (including) *)
let from s p = from_to s p (String.length s)

(** safe (lazy) way to cut substring
  same as extlib [String.slice] *)
let slice =
  let clip max x = if x > max then max else if x < 0 then 0 else x in
  fun ?(first=0) ?(last=Sys.max_string_length) s ->
    let len = String.length s in
    let i = if first = 0 then 0 else clip len (if first < 0 then len + first else first) in
    let j = if last = Sys.max_string_length then len else clip len (if last < 0 then len + last else last) in
    if i < j then unsafe_from_to s i j else ""

(** split by delimiter
  @raise Not_found if [sep] is not found in [str] *)
let splitc str sep =
  let p = String.index str sep in
  String.sub str 0 p, String.sub str (p+1) (String.length str - p - 1)

(** split by delimiter from the right
  @raise Not_found if [sep] is not found in [str] *)
let rsplitc str sep =
  let p = String.rindex str sep in
  String.sub str 0 p, String.sub str (p+1) (String.length str - p - 1)

(** [before s sep]
  @return substring from the beginning of [s] to the delimiter [sep] or the original string if delimiter is not present.
  NB [before s "" -> ""]
  e.g. [before "a.b.c" "." -> "a"]
*)
let before s sep = try String.sub s 0 (String.find s sep) with ExtString.Invalid_string -> s

(** @return substring from the delimiter [sep] to the end of string [s] or the empty string if delimiter is not present.
  e.g. [after "a.b.c" "." -> "b.c"]
  NB [after s "" -> s]
  invariant:
    if [s] contains [sep] then [before s sep ^ sep ^ after s sep = s] else [before s sep ^ after s sep = s]
  or put it another way:
    say [a = before s sep] and [b = after s sep] then
    [before (a ^ sep ^ b) sep = a] and [after (a ^ sep ^ b) sep = b]
  *)
let after s sep = try String.(let i = find s sep + length sep in sub s i (length s - i)) with ExtString.Invalid_string -> ""

(** @return [(before s sep, after s sep)]
  [divide s sep] is equal to [String.split] but doesn't throw if [sep] is not a substring of [s]
*)
let divide s sep = try String.split s sep with Invalid_string -> s, ""
let dividec s sep = try splitc s sep with Not_found -> s, ""

let qreplace str sub by =
  Pcre.qreplace ~rex:(Pcre.regexp @@ Pcre.quote sub) ~templ:by str

let replace_all ~str ~sub ~by = qreplace str sub by

(** contents of the first submatch *)
let extract rex str =
  try
    Some (Pcre.extract ~rex ~full_match:false str).(0)
  with
    _ -> None

(** String.starts_with but with [pos] to start from *)
let starts_with s ?(pos=0) prefix =
  if pos < 0 || pos > String.length s then invalid_arg "Stre.starts_with";
  if String.length s < pos + String.length prefix then false else
  try
    for i = 0 to String.length prefix - 1 do
      if s.[pos + i] <> prefix.[i] then raise_notrace Not_found
    done;
    true
  with Not_found ->
    false

let ends_with s suffix = String.ends_with s suffix[@warning "-6"]
let exists s sub = String.exists s sub[@warning "-6"]

(** remove prefix from string if present *)
let drop_prefix s pre = if starts_with s pre then slice s ~first:(String.length pre) else s
let drop_suffix s suf = if ends_with s suf then slice ~last:(- String.length suf) s else s

let istarts_with s ?(pos=0) prefix =
  if pos < 0 || pos > String.length s then invalid_arg "Stre.istarts_with";
  if String.length s < pos + String.length prefix then false else
  try
  for i = 0 to String.length prefix - 1 do
    let c1 = s.[pos + i] and c2 = prefix.[i] in
    if c1 <> c2 && Char.lowercase_ascii c1 <> Char.lowercase_ascii c2 then raise_notrace Not_found
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
    if istarts_with s ~pos:i sub then raise_notrace Exit;
  done; false
  with Exit -> true

(** sequence of matches *)
let enum_matches rex s =
  try
    Pcre.exec_all ~rex s |> Array.enum
  with
    _ -> Enum.empty ()

let enum_extract rex s = enum_matches rex s |> Enum.map (flip Pcre.get_substring 1)

module ASCII = struct
let is_alpha = function
| 'a'..'z' | 'A'..'Z' -> true
| _ -> false
let is_digit = function
| '0'..'9' -> true
| _ -> false
let is_alnum c = is_alpha c || is_digit c
end

let rev = String.implode $ List.rev $ String.explode

let common_prefix s1 s2 =
  let i = ref 0 in
  let min_len = min (String.length s1) (String.length s2) in
  while !i < min_len && s1.[!i] = s2.[!i] do incr i done;
  !i

let shorten ?(escape=false) limit s =
  let escape = if escape then String.escaped else id in
  let limit = max limit 24 in
  if String.length s <= limit then escape s
  else
    let limit = limit - 20 in
    Printf.sprintf "%s..[+%d bytes]..%s" (escape @@ slice ~last:limit s) (String.length s - limit - 4) (escape @@ slice ~first:(-4) s)

let array_concat sep a =
  match a with
  | [||] -> ""
  | _ ->
    let b = Buffer.create ((String.length a.(0) + String.length sep) * Array.length a) in
    Buffer.add_string b a.(0);
    for i = 1 to Array.length a - 1 do
      Buffer.add_string b sep;
      Buffer.add_string b a.(i);
    done;
    Buffer.contents b

let rstrip ?(chars=" \t\r\n") s =
  let l = ref (String.length s - 1) in
  while !l >= 0 && String.contains chars (String.unsafe_get s !l) do
    decr l;
  done;
  String.sub s 0 (!l + 1)

let catmap ?(sep="") f l = String.concat sep (List.map f l)
let list f l = sprintf "[%s]" @@ catmap ~sep:";" f l
let array f a = sprintf "[|%s|]" @@ array_concat ";" @@ Array.map f a
