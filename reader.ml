
open ExtLib
open String

(* invariant: 0 <= pos <= String.length s *)
type t = { s : string; mutable pos : int; }

exception EOS
exception Not_equal of string

let init s = { s; pos = 0; }

let left t = length t.s - t.pos
let eos t = left t = 0

let rest t =
  let s = sub t.s t.pos (left t) in
  t.pos <- length t.s;
  s

let till t sep =
  try
    let i = find_from t.s t.pos sep in
    let s = sub t.s t.pos (i - t.pos) in
    t.pos <- i + length sep;
    s
  with
    Invalid_string -> raise EOS

let try_till t sub = try till t sub with EOS -> rest t

let tillc t c =
  try
    let i = index_from t.s t.pos c in
    let s = sub t.s t.pos (i - t.pos) in
    t.pos <- i + 1;
    s
  with
    Invalid_string -> raise EOS

let try_tillc t c = try tillc t c with EOS -> rest t

let extract t n =
  let s = sub t.s t.pos n in
  t.pos <- t.pos + n;
  s

let take t n =
  if n > left t then raise EOS;
  extract t n

let while_ t p =
  let rec loop t p i =
    if i = length t.s then rest t else
    if p @@ String.unsafe_get t.s i then loop t p (i+1)
    else extract t (i - t.pos)
  in
  loop t p t.pos

let skipc t c =
  if t.pos = length t.s then raise EOS;
  if String.unsafe_get t.s t.pos = c then t.pos <- t.pos + 1 else raise (Not_equal (String.make 1 c))

let try_take t n = take t (min n (left t))

let is_const t s = length s <= left t && sub t.s t.pos (length s) = s

let const t s =
  if length s > left t then raise EOS;
  if sub t.s t.pos (length s) <> s then raise (Not_equal s);
  t.pos <- t.pos + length s

let try_const t s =
  let s = if length s <= left t then s else sub s 0 (left t) in
  const t s
