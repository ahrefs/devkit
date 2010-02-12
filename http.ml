
open Prelude

open Printf
module Ev = Liboevent

let log = Log.from "http"

DEFINE INC(x) = x <- x + 1
DEFINE DEC(x) = x <- x - 1

(** Create persistent event. Don't forget [del] *)
let event fd flags f =
  let ev = Ev.create () in
  Ev.set ev fd flags ~persist:true (fun fd flags -> f ev fd flags);
  Ev.add ev None

type request = { addr : Unix.sockaddr ;
                 mutable url : string option ;
                 conn : Time.t ; (* time when client connected *)
                 mutable recv : Time.t ; (* time when client request was read *)
                 }

let show_client c =
  sprintf "%s time %.4f (recv %.4f) url %s" 
    (Nix.string_of_sockaddr c.addr) 
    (Time.get () -. c.conn) 
    (c.recv -. c.conn) 
    (Option.default "?" c.url)

type status = { mutable reqs : int; mutable active : int; mutable errs : int; }

let parse_http_req s =
  try
  Scanf.sscanf s "%s@  %s@  %s@\r\n" & fun meth url version ->
  match meth,version with
  | "GET","HTTP/1.0" | "GET","HTTP/1.1" -> Some url
  | _ -> None
  with
  End_of_file -> None

let int_of_fd : Unix.file_descr -> int = Obj.magic

let close fd =
(*   Log.info "close %u" (int_of_fd fd); *)
  Exn.suppress (Unix.shutdown fd) Unix.SHUTDOWN_ALL;
  Unix.close fd

let write_f status client (data,ack) ev fd _flags =
  let finish () =
    log #debug "finished %s" & show_client client;
    Ev.del ev; 
    close fd;
    DEC status.active
  in
  let rec loop l ack =
    match l with
    | [] -> finish (); ([],0)
    | s::xs when String.length s = 0 -> loop xs 0 (* skip empty strings *)
    | s::xs ->
      try
        let len = Unix.single_write fd s ack (String.length s - ack) in
        let ack = ack + len in
        if ack = String.length s then
          loop xs 0
        else 
          loop l ack
      with
      | Unix.Unix_error (Unix.EAGAIN,_,_) -> (l,ack)
  in
  try
    let (l,a) = loop !data !ack in
    data := l;
    ack := a
  with
  exn -> 
    Exn.suppress Ev.del ev;
    Exn.suppress close fd;
    INC status.errs;
    DEC status.active;
    log #warn ~exn "write_f %s" & show_client client

let http_reply = function
  | `Ok -> "HTTP/1.0 200 OK"
  | `Not_found -> "HTTP/1.0 404 Not Found"
  | `Bad_request -> "HTTP/1.0 400 Bad Request"

let read_all fd =
  let read buf =
    try
      match Unix.read fd buf 0 (String.length buf) with
      | 0 -> None
      | n -> Some n
    with
    | Unix.Unix_error (Unix.EAGAIN,_,_) -> None
  in
  let buf = Buffer.create 1024 in
  let s = String.create 1024 in
  let rec loop () =
    match read s with
    | None -> Buffer.contents buf
    | Some len -> Buffer.add_substring buf s 0 len; loop ()
  in
  loop ()

let handle_client status fd client answer =
  INC status.reqs;
  INC status.active;
  Unix.set_nonblock fd;
  event fd [Ev.READ] begin fun ev fd _ ->
    try
    Ev.del ev; 
    let request = read_all fd in (* FIXME may not read whole request *)
(*     Log.info "fd %u got %u bytes" (int_of_fd fd) len; *)
    client.url <- parse_http_req request;
    client.recv <- Time.get ();
    let (code,hdrs,body) = answer status client.url in
    let b = Buffer.create 1024 in
    let put s = Buffer.add_string b s; Buffer.add_string b "\r\n" in
    put (http_reply code);
    List.iter put hdrs;
    bprintf b "Content-length: %u\r\n" (String.length body);
    put "Connection: close";
    put "";
(*     Buffer.add_string b body; *)
    event fd [Ev.WRITE] (write_f status client (ref [Buffer.contents b; body],ref 0))
    with
    exn -> 
      (*Exn.suppress Ev.del ev;*)
      INC status.errs;
      DEC status.active;
      Exn.suppress close fd;
      log #warn ~exn "handle_client %s" & show_client client
  end

let server addr answer = 
  open Unix in
  let fd = socket PF_INET SOCK_STREAM 0 in
  set_nonblock fd;
  setsockopt fd SO_REUSEADDR true;
  bind fd addr;
  listen fd 100;
  let status = { reqs = 0; active = 0; errs = 0; } in
  event fd [Ev.READ] begin fun _ fd _ -> 
(*     Log.info "client";  *)
    try
      let (fd,addr) = accept fd in
      handle_client status fd { addr=addr; conn=Time.get(); recv=0.; url=None } answer
    with
      exn -> log #error ~exn "accept"
  end;
  Ev.dispatch ()

let header n v = sprintf "%s: %s" n v
let bad_request = `Bad_request, [], "bad request"
let not_found = `Not_found, [], "not found"

let server addr answer =
  server addr (fun status url ->
    match url with
    | None -> bad_request 
    | Some url -> answer status url)

(*
let answer = function
  | None -> bad_request, "Bad request"
  | Some url ->
    let answer = sprintf "answer %s\n%s" url (String.create 102400) in
    [status `Ok], answer

let () =
  server (Unix.ADDR_INET (Unix.inet_addr_any, 8081)) answer

*)
