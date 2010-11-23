(** Asynchronous IO helpers *)

open Prelude

module Ev = Libevent

module Internal = struct
let log = Log.from "async"
end

open Internal

(** Create persistent event. Don't forget [del]. No (infinite) timeout. *)
let simple_event events ?timeout fd flags f =
  let ev = Ev.create () in
  Ev.set events ev fd flags ~persist:true (fun fd flags ->
    try
      f ev fd flags
    with
      exn -> log #warn ~exn "simple_event");
  Ev.add ev timeout;
  ev

let setup_simple_event events ?timeout fd flags f =
  let (_:Ev.event) = simple_event events ?timeout fd flags f in
  ()

type result = End | Data of int | Block | Exn of exn

let read_some fd buf ofs len =
  try
    match Unix.read fd buf ofs len with
    | 0 -> End
    | n -> Data n
  with
  | Unix.Unix_error ((Unix.EAGAIN|Unix.EWOULDBLOCK),_,_) -> Block
  | exn -> log #warn ~exn "read_some"; Exn exn

let write_some fd buf ofs len =
  try
    Unix.write fd buf ofs len
  with
  | Unix.Unix_error ((Unix.EAGAIN|Unix.EWOULDBLOCK),_,_) -> 0

(** Read out all immediately available input (no blocking) *)
let read_available ~limit fd =
  let buf = Buffer.create 1024 in
  let s = String.create 1024 in
  let rec loop () =
    match read_some fd s 0 (String.length s) with
    | End -> `Done (Buffer.contents buf)
    | Block -> `Part (Buffer.contents buf)
    | Exn exn -> raise exn
    | Data len ->
      Buffer.add_substring buf s 0 len;
      if Buffer.length buf > limit then `Limit (Buffer.contents buf) else loop ()
  in
  loop ()

(** [read_buf buf fd err k] - asynchronously fill [buf] with data from [fd] and call [k buf] when done (buffer is full).
  [fd] should be nonblocking. Call [err] on error (EOF). *)
let read_buf base ?timeout buf fd err k =
  let len = String.length buf in
  let later cur =
    let cur = ref cur in
    setup_simple_event base ?timeout fd [Ev.READ] (fun ev fd flags ->
      match flags with
      | Ev.TIMEOUT -> Ev.del ev; err ()
      | Ev.WRITE | Ev.SIGNAL -> assert false
      | Ev.READ ->
      match read_some fd buf !cur (len - !cur) with
      | End -> Ev.del ev; err ()
      | Exn exn -> Ev.del ev; err ()
      | Data n -> cur := !cur + n; if !cur = len then begin Ev.del ev; k buf end
      | Block -> assert false
    )
  in
  match read_some fd buf 0 len with
  | End -> err ()
  | Exn exn -> err ()
  | Data n when n = len -> k buf
  | Block -> later 0
  | Data n -> later n

let read_n base ?timeout n fd err k = read_buf base ?timeout (String.create n) fd err k

(** Call [f] with [delay]-second pauses between invocations *)
let periodic_timer events delay ?(name="") f =
  let timer = Ev.create () in
  Ev.set_timer events timer ~persist:false begin fun () ->
    begin try f timer with exn -> log #warn ~exn "periodic_timer %s" name end;
    Ev.add timer (Some delay);
  end;
  Ev.add timer (Some 0.);
  timer

let setup_periodic_timer events delay ?name f =
  let (_:Ev.event) = periodic_timer events delay ?name f in
  ()

module Peer = struct

type t = { events : Ev.event_base; read : Ev.event; write : Ev.event; timeout : float option; fd : Unix.file_descr; addr : Unix.sockaddr; err : (unit -> unit); }

let create events ?(err=id) ?timeout (fd,addr) =
  Unix.set_nonblock fd;
  { events = events; fd = fd; addr = addr; timeout = timeout; read = Ev.create (); write = Ev.create (); err = err; }

let add_event p ev timeout =
 let timeout = if timeout = None then p.timeout else timeout in
 Ev.add ev timeout

let finish p =
  Ev.del p.read; Ev.del p.write;
  begin try Unix.shutdown p.fd Unix.SHUTDOWN_ALL with _ -> () end;
  Unix.close p.fd

let error ?exn p msg =
  log #warn ?exn "Peer %s %s" (Nix.show_addr p.addr) msg;
  Std.finally (fun () -> finish p) p.err ()

let receive p ?timeout buf k =
  let len = String.length buf in
  let later cur =
    let cur = ref cur in
    Ev.set p.events p.read p.fd [Ev.READ] ~persist:true (fun fd flags ->
      try
        match flags with
        | Ev.TIMEOUT -> error p "receive timeout"
        | Ev.WRITE -> assert false
        | Ev.SIGNAL -> assert false
        | Ev.READ ->
          match read_some fd buf !cur (len - !cur) with
          | End -> error p "receive eof"
          | Exn exn -> error ~exn p "receive"
          | Data n -> cur := !cur + n; if !cur = len then (Ev.del p.read; k buf)
          | Block -> assert false
      with
        exn -> error ~exn p "receive");
    add_event p p.read timeout
  in
  later 0
(*
  match read_some p.fd buf 0 len with
  | End -> error p "receive eof"
  | Exn exn -> error ~exn p "receive"
  | Data n when n = len -> k buf
  | Block -> later 0
  | Data n -> later n
*)

let send p ?timeout buf k =
  let len = String.length buf in
  let later cur =
    let cur = ref cur in
    Ev.set p.events p.write p.fd [Ev.WRITE] ~persist:true (fun fd flags ->
      try
        match flags with
        | Ev.TIMEOUT -> error p "send timeout"
        | Ev.READ -> assert false
        | Ev.SIGNAL -> assert false
        | Ev.WRITE ->
          cur := !cur + write_some fd buf !cur (len - !cur);
          if !cur = len then (Ev.del p.write; k ())
      with
        exn -> error ~exn p "send");
    add_event p p.write timeout
  in
  later 0
(*
  match try Some (write_some p.fd buf 0 len) with exn -> error ~exn p "send"; None with
  | Some n when n = len -> k () 
  | Some n -> later n
  | None -> ()
*)

let send_all p ?timeout bufs k =
  let rec loop = function
  | [] -> k ()
  | x::xs -> send p ?timeout x (fun () -> loop xs)
  in
  loop bufs

let connect p ?timeout k =
  try
    Unix.connect p.fd p.addr
  with
  | Unix.Unix_error(Unix.EINPROGRESS,_,_) ->
  Ev.set p.events p.read p.fd [Ev.READ] ~persist:false (fun fd flags ->
    try
      match flags with
      | Ev.TIMEOUT -> error p "connect timeout"
      | Ev.WRITE -> assert false
      | Ev.SIGNAL -> assert false
      | Ev.READ ->
        match Unix.getsockopt_error fd with
        | Some err -> error p ~exn:(Unix.Unix_error (err,"connect","")) "connect"
        | None -> k ()
    with
      exn -> error ~exn p "connect");
  add_event p p.read timeout
  | exn -> error ~exn p "connect"

end (* Peer *)

