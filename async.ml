
open Prelude

module Ev = Liboevent

module Internal = struct
let log = Log.from "async"
end

open Internal

(** Create persistent event. Don't forget [del]. No (infinite) timeout. *)
let simple_event base fd flags f =
  let ev = Ev.create () in
  Ev.set ev fd flags ~persist:true (fun fd flags ->
    try
      f ev fd flags
    with
      exn -> log #warn ~exn "event");
  Ev.add base ev None

type result = End | Data of int | Block

(** Read out all immediately available input (no blocking) *)
let read_available ~limit fd =
  let read buf =
    try
      match Unix.read fd buf 0 (String.length buf) with
      | 0 -> End
      | n -> Data n
    with
    | Unix.Unix_error (Unix.EAGAIN,_,_) -> Block
  in
  let buf = Buffer.create 1024 in
  let s = String.create 1024 in
  let rec loop () =
    match read s with
    | End -> `Done (Buffer.contents buf)
    | Block -> `Part (Buffer.contents buf)
    | Data len ->
      Buffer.add_substring buf s 0 len;
      if Buffer.length buf > limit then `Limit (Buffer.contents buf) else loop ()
  in
  loop ()

