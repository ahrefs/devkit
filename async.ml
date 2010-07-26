(** Asynchronous IO helpers *)

open Prelude

module Ev = Libevent

module Internal = struct
let log = Log.from "async"
end

open Internal

let add_simple_event ev ?timeout fd flags f =
  Ev.set ev fd flags ~persist:true f;
  Ev.add ev timeout

(** Create persistent event. Don't forget [del]. No (infinite) timeout. *)
let simple_event events ?timeout fd flags f =
  let ev = Ev.create events in
  add_simple_event ev ?timeout fd flags (fun fd flags ->
    try
      f ev fd flags
    with
      exn -> log #warn ~exn "event")

type result = End | Data of int | Block

let read_some fd buf ofs len =
  try
    match Unix.read fd buf ofs len with
    | 0 -> End
    | n -> Data n
  with
  | Unix.Unix_error (Unix.EAGAIN,_,_) -> Block

(** Read out all immediately available input (no blocking) *)
let read_available ~limit fd =
  let buf = Buffer.create 1024 in
  let s = String.create 1024 in
  let rec loop () =
    match read_some fd s 0 (String.length s) with
    | End -> `Done (Buffer.contents buf)
    | Block -> `Part (Buffer.contents buf)
    | Data len ->
      Buffer.add_substring buf s 0 len;
      if Buffer.length buf > limit then `Limit (Buffer.contents buf) else loop ()
  in
  loop ()

(** [read_buf buf fd err k] - asynchronously fill [buf] with data from [fd] and call [k buf] when done (buffer is full).
  [fd] should be nonblocking. Call [err] on error (EOF). *)
let read_buf base buf fd err k =
  let len = String.length buf in
  let later cur =
    let cur = ref cur in
    simple_event base fd [Ev.READ] (fun ev fd flags ->
      match read_some fd buf !cur (len - !cur) with
      | End -> Ev.del ev; err ()
      | Data n -> cur := !cur + n; if !cur = len then begin Ev.del ev; k buf end
      | Block -> Ev.del ev; assert false
    )
  in
  match read_some fd buf 0 len with
  | End -> err ()
  | Data n when n = len -> k buf
  | Block -> later 0
  | Data n -> later n

let read_n base n fd err k = read_buf base (String.create n) fd err k

let add_periodic_timer timer delay ?(name="") f =
  let loop () =
    try 
      if f () then
        Ev.add timer (Some delay)
    with exn -> log #warn ~exn "periodic_timer %s" name
  in
  Ev.set_timer timer ~persist:false loop;
  Ev.add timer (Some 0.)

let setup_periodic_timer events delay ?(name="") f =
  let ev = Ev.create events in
  add_periodic_timer ev delay ~name (fun () ->
    begin try f () with exn -> log #warn ~exn "periodic_timer %s" name end; 
    true)

