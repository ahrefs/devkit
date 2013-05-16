
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

let take t n =
  if n > left t then raise EOS;
  let s = sub t.s t.pos n in
  t.pos <- t.pos + n;
  s

let try_take t n = take t (min n (left t))

let is_const t s = length s <= left t && sub t.s t.pos (length s) = s

let const t s =
  if length s > left t then raise EOS;
  if sub t.s t.pos (length s) <> s then raise (Not_equal s);
  t.pos <- t.pos + length s

let try_const t s =
  let s = if length s <= left t then s else sub s 0 (left t) in
  const t s
