open Bigarray
module Bytes = Array1

let bget t pos = int_of_char @@ Bytes.unsafe_get t pos
let bset t pos c = Bytes.unsafe_set t pos (Char.unsafe_chr c)

type t = (char, int8_unsigned_elt, c_layout) Bytes.t

let int_size = 7 (* value used to round up index *)
let log_int_size = 3 (* number of shifts *)

let create n =
  let size = (n+int_size) lsr log_int_size in
  let b = Bytes.create Char C_layout size in
  Bytes.fill b '\x00';
  b

let copy t =
  let b = Bytes.create Char C_layout (Bytes.dim t) in
  Bytes.blit t b;
  b

let set t x =
  let pos = x lsr log_int_size and delta = x land int_size in
  bset t pos ((bget t pos) lor (1 lsl delta))

let unset t x =
  let pos = x lsr log_int_size and delta = x land int_size in
  bset t pos ((bget t pos) land (0xFF lxor (1 lsl delta)))

let toggle t x =
  let pos = x lsr log_int_size and delta = x land int_size in
  bset t pos ((bget t pos) lxor (1 lsl delta))

let put t = function
  | true -> set t
  | false -> unset t

let is_set t x =
  let pos = x lsr log_int_size and delta = x land int_size in
  0 <> (((bget t pos) lsr delta) land 1)
