(** Very simple and incomplete HTTP server *)

open Printf
open ExtLib
module Ev = Liboevent

open Prelude

let log = Log.from "httpev"

DEFINE INC(x) = x <- x + 1
DEFINE DEC(x) = x <- x - 1

(** {2 Server} *)

(** Create persistent event. Don't forget [del] *)
let event fd flags f =
  let ev = Ev.create () in
  Ev.set ev fd flags ~persist:true (fun fd flags -> f ev fd flags);
  Ev.add ev None

type request = { addr : Unix.sockaddr ;
                 url : string;
                 path : string ;
                 args : (string * string) list;
                 conn : Time.t; (* time when client connected *)
                 recv : Time.t; (* time when client request was fully read *)
                 meth : string; (* HTTP method *)
                 headers : (string * string) list;
                 body : string;
                 version : string; (* HTTP version *)
                 }

let show_request c =
  sprintf "%s time %.4f (recv %.4f) url %s" 
    (Nix.string_of_sockaddr c.addr) 
    (Time.get () -. c.conn) 
    (c.recv -. c.conn) 
    c.url

type status = { mutable reqs : int; mutable active : int; mutable errs : int; }

type ('a,'b) result = [ `Ok of 'a | `Error of 'b ]

let space = Pcre.regexp "[ \t]+"

let parse_http_req s (addr,conn) =
  let next s = String.split s "\r\n" in
  try
    let (line,s) = next s in
    match Pcre.split ~rex:space line with
    | [meth;url;version] ->
      let rec loop hs s =
        match next s with
        | "",body ->
          let (path,args) = try String.split url "?" with _ -> url,"" in
          let args = Netencoding.Url.dest_url_encoded_parameters args in
          {
            addr = addr;
            url = url;
            path = path;
            args = args;
            conn = conn;
            recv = Time.get ();
            headers = List.rev_map (fun (n,v) -> String.lowercase n,v) hs;
            body = body;
            meth = meth;
            version = version;
          }
        | line,s ->
          let (n,v) = String.split line ":" in
          loop ((n, String.strip v) :: hs) s
      in
      `Ok (loop [] s)
    | _ -> Exn.fail "bad request line"
  with
  exn -> `Error exn

let int_of_fd : Unix.file_descr -> int = Obj.magic

let close fd =
(*   Log.info "close %u" (int_of_fd fd); *)
  Exn.suppress (Unix.shutdown fd) Unix.SHUTDOWN_ALL;
  Unix.close fd

let write_f status req (data,ack) ev fd _flags =
  let finish () =
    log #debug "finished %s" & Option.map_default show_request "" req;
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
    log #warn ~exn "write_f %s" & Option.map_default show_request "" req

let http_reply = function
  | `Ok -> "HTTP/1.0 200 OK"
  | `Not_found -> "HTTP/1.0 404 Not Found"
  | `Unauthorized -> "HTTP/1.0 401 Unauthorized"
  | `Bad_request -> "HTTP/1.0 400 Bad Request"
  | `Forbidden -> "HTTP/1.0 403 Forbidden"
  | `Request_too_large -> "HTTP/1.0 413 Request Entity Too Large"
  | `Internal_server_error -> "HTTP/1.0 500 Internal Server Error"
  | `Custom s -> s

let read_all ~limit fd =
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
    | None -> `Ok (Buffer.contents buf)
    | Some len -> 
      Buffer.add_substring buf s 0 len;
      if Buffer.length buf > limit then `Error `Limit else loop ()
  in
  loop ()

let handle_client status fd conn_info answer =
  INC status.reqs;
  INC status.active;
  Unix.set_nonblock fd;
  event fd [Ev.READ] begin fun ev fd _ ->
    try
    Ev.del ev; 
    let (req,(code,hdrs,body)) =
    (* FIXME may not read whole request *)
    match read_all ~limit:(16*1024) fd with
    | `Error `Limit -> None, (`Request_too_large,[],"request entity too large")
    | `Ok data ->
      log #debug "read_all: %d bytes" (String.length data);
      match parse_http_req data conn_info with
      | `Error exn ->
        log #warn ~exn "Failed to parse request";
        None, (`Bad_request,[],"bad request")
      | `Ok req ->
        try
          Some req, answer status req
        with exn ->
          log #error ~exn "answer %s" & show_request req;
          None, (`Internal_server_error,[],"Internal server error")
    in
    let b = Buffer.create 1024 in
    let put s = Buffer.add_string b s; Buffer.add_string b "\r\n" in
    put (http_reply code);
    List.iter (fun (n,v) -> bprintf b "%s: %s\r\n" n v) hdrs;
    bprintf b "Content-length: %u\r\n" (String.length body);
    put "Connection: close";
    put "";
    log #debug "will answer with %d+%d bytes" 
      (Buffer.length b) 
      (String.length body);
(*     Buffer.add_string b body; *)
    event fd [Ev.WRITE] 
      (write_f status req (ref [Buffer.contents b; body],ref 0))
    with
    exn -> 
      (*Exn.suppress Ev.del ev;*)
      INC status.errs;
      DEC status.active;
      Exn.suppress close fd;
      let (addr,stamp) = conn_info in
      log #warn ~exn "handle_client %s %.4f" 
        (Nix.string_of_sockaddr addr) stamp
  end

include struct
open Unix

let server addr answer = 
(*   open Unix in *)
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
      handle_client status fd (addr,Time.get()) answer
    with
      exn -> log #error ~exn "accept"
  end;
  Ev.dispatch ()

end

let header n v = n,v
let forbidden = `Forbidden, [], "forbidden"
let not_found = `Not_found, [], "not found"

(*
let answer st url =
  match url with
  | "/test" -> 
    let body = sprintf "answer %s\n%s" url (String.create 102400) in
    `Ok,[],answer
  | _ -> not_found

let () =
  server (Unix.ADDR_INET (Unix.inet_addr_any, 8081)) answer
*)

(** {2 Utilities} 
  mimic {!Netcgi_ext} interface
*)

module Args(T : sig val req : request end) =
struct
  let arg name = List.assoc name T.req.args
  exception Bad of string
  let get name = Exn.catch arg name
  let str name = match get name with Some s -> s | None -> raise (Bad name)
  let int name = let s = str name in try int_of_string s with _ -> raise (Bad name)
end

(** Buffers all output *)
let output (f : 'a IO.output -> unit) =
  let out = IO.output_string () in
  f (Netcgi_ext.noclose out);
  IO.close_out out

let serve (_req : request) ?status ctype data =
  Option.default `Ok status, ["Content-Type",ctype], data

let serve_io (req : request) ?status ctype (f : 'a IO.output -> unit) =
  serve req ?status ctype (output f)

let serve_text_io req ?status =
  serve_io req ?status "text/plain"

let serve_gzip_io req ?status f =
  serve_io req ?status "application/gzip" (fun io ->
    Control.with_output (Gzip_io.output io) f)

let serve_text req ?status text = 
  serve req ?status "text/plain" text

let serve_html req html =
  serve_io req "text/html" (fun out -> 
    XHTML.M.pretty_print (IO.nwrite out) html)

let run ?(addr=Unix.inet_addr_loopback) port answer =
  log #info "Ready for HTTP on %s:%u" (Unix.string_of_inet_addr addr) port;
  server (Unix.ADDR_INET (addr,port)) answer

let input_header req name =
  List.assoc (String.lowercase name) req.headers

