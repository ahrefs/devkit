type t = {
  mutable buffer : Bytes.t;
  mutable buffer_length : int; (* = String.length buffer *)
  mutable length : int;
  create_length : int;
}

(* To help the garbage collector:
 * The 'buffer' has a minimum length of 31 bytes. This minimum can still
 * be stored in the minor heap.
 * The 'buffer' has a length which is always near a multiple of two. This
 * limits the number of different bucket sizes, and simplifies reallocation
 * of freed memory.
 *)

(* Optimal string length:
 * Every string takes: 1 word for the header, enough words for the 
 * contents + 1 Null byte (for C compatibility).
 * If the buffer grows, it is best to use a new string length such
 * that the number of words is exactly twice as large as for the previous
 * string.
 * n:              length of the previous string in bytes
 * w:              storage size of the previous string in words
 * n':             length of the new string in bytes
 * w' = 2*w:       storage size of the new string in words
 *
 * w = (n+1) / word_length + 1
 *            [it is assumed that (n+1) is always a multiple of word_length]
 *
 * n' = (2*w - 1) * word_length - 1
 *
 * n' = [2 * ( [n+1] / word_length + 1) - 1] * word_length - 1
 *    = ...
 *    = (2*n + 2) + word_length - 1
 *    = 2 * n + word_length + 1
 *
 * n'+1 is again a multiple of word_length:
 * n'+1 = 2*n + 2 + word_length
 *      = 2*(n+1) + word_length
 *      = a multiple of word_length because n+1 is a multiple of word_length
 *)

let word_length = Sys.word_size / 8 (* in bytes *)

let create n =
  let bl = max n 31 in
  {
    buffer = Bytes.create bl;
    buffer_length = bl;
    length = 0;
    create_length = n;
  }

let contents b = Bytes.sub_string b.buffer 0 b.length
let to_bytes b = Bytes.sub b.buffer 0 b.length

let to_tstring_poly : type s. t -> s Netstring_tstring.tstring_kind -> s =
 fun b kind ->
  match kind with
  | Netstring_tstring.String_kind -> contents b
  | Netstring_tstring.Bytes_kind -> to_bytes b

let alloc_space b n =
  let rec new_size s =
    if s >= n then s else new_size ((2 * s) + word_length + 1)
  in
  let size = min (new_size b.buffer_length) Sys.max_string_length in
  if size < n then failwith "Netbuffer: string too large";
  let buffer' = Bytes.create size in
  Bytes.blit b.buffer 0 buffer' 0 b.length;
  b.buffer <- buffer';
  b.buffer_length <- size

let ensure_space b n =
  (* Ensure that there are n bytes space in b *)
  if n > b.buffer_length then alloc_space b n

let add_internal blit b s k l =
  ensure_space b (l + b.length);
  blit s k b.buffer b.length l;
  b.length <- b.length + l

let add_substring b s k l =
  if k < 0 || l < 0 || k > String.length s - l then
    invalid_arg "Netbuffer.add_substring";
  add_internal Bytes.blit_string b s k l

let add_string b s = add_substring b s 0 (String.length s)
