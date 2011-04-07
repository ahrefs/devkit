(** Safe marshalling *)

open Control
open ExtLib

module type Value =
sig
type value
val tag : string
end

exception Error

module Marshal(V : Value) =
struct

type t = V.value

let to_channel ch ?(flags=[]) x =
  output_string ch V.tag;
  Marshal.to_channel ch (x:t) flags

let from_channel ch =
  let s = String.create (String.length V.tag) in
  really_input ch s 0 (String.length V.tag);
  if s <> V.tag then raise Error;
  (Marshal.from_channel ch : t)

let to_string ?(flags=[]) x = V.tag ^ Marshal.to_string (x:t) flags
let from_string s = 
  let tag = String.slice s ~last:(String.length V.tag) in
  if tag <> V.tag then raise Error;
  (Marshal.from_string s (String.length V.tag) : t)

let to_file_exn name ?mode ?(flags=[]) x =
  Files.save_as name ?mode (fun ch -> to_channel ch ~flags x)

let from_file name =
  with_open_in_bin name from_channel

end

