(** Safe marshalling *)

open Control

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

let to_file name ?(mode=0o644) ?(flags=[]) x =
  let temp = name^".temp" in
  bracket (Unix.openfile temp [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_EXCL] mode) Unix.close begin fun fd ->
    try
      let ch = Unix.out_channel_of_descr fd in
      to_channel ch ~flags x;
      flush ch;
      Nix.fsync fd;
      Unix.rename temp name
    with
      exn -> Log.main #warn "to_file %s" name; Exn.suppress Unix.unlink temp
  end
(*     with_open_out_bin name (fun ch -> to_channel ch ~flags x) *)

let from_file name =
  with_open_in_bin name from_channel

end

