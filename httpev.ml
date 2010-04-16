(** Very simple and incomplete HTTP server *)

open Printf
open ExtLib
module Ev = Liboevent

open Prelude

(* hide log *)
module M = struct let log = Log.from "httpev" end
open M

DEFINE INC(x) = x <- x + 1
DEFINE DEC(x) = x <- x - 1

(** {2 Server} *)

(** server configuration *)
type config =
  {
    ip : Unix.inet_addr;
    port : int;
    backlog : int;
    log_epipe : bool;
    events : Ev.event_base;
  }

let default = 
  {
    ip = Unix.inet_addr_loopback;
    port = 8080;
    backlog = 100;
    log_epipe = false;
    events = Ev.Global.base;
  }

type request = { addr : Unix.sockaddr ;
                 url : string; (* path and arguments *)
                 path : string;
                 args : (string * string) list;
                 conn : Time.t; (* time when client connected *)
                 recv : Time.t; (* time when client request was fully read *)
                 meth : [`GET | `POST | `HEAD ];
                 headers : (string * string) list;
                 body : string;
                 version : int * int; (* client HTTP version *)
                 }

let show_request c =
  let client = Nix.show_addr c.addr in
  let client = try sprintf "%s (%s)" (List.assoc "x-real-ip" c.headers) client with Not_found -> client in
  sprintf "%s time %.4f (recv %.4f) %s%s"
    client
    (Time.get () -. c.conn)
    (c.recv -. c.conn)
    (Exn.default "" (List.assoc "host") c.headers)
    c.url

type status = { mutable reqs : int; mutable active : int; mutable errs : int; }

type ('a,'b) result = [ `Ok of 'a | `Error of 'b ]

let space = Pcre.regexp "[ \t]+"

type reason = Url | Version | Method | Header | RequestLine | Split | Length
exception Parse of reason * string
let failed reason s =
  let name =
  match reason with
  | Url -> "url" | Version -> "version" | Method -> "method"
  | RequestLine -> "RequestLine" | Split -> "split" | Header -> "header" | Length -> "length"
  in
  let lim = 1024 in
  let s = if String.length s > lim then sprintf "%s [%d bytes more]" (String.slice ~last:lim s) (String.length s - lim) else s in
  raise (Parse (reason, sprintf "%s : %s" name s))

let parse_http_req data (addr,conn) =
  let next s = try String.split s "\r\n" with _ -> failed Split data in
  try
    let (line,s) = next data in
    match Pcre.split ~rex:space line with
    | [meth;url;version] ->
      if url.[0] <> '/' then (* abs_path *)
        failed Url url;
      let version = 
        try
          Scanf.sscanf version "HTTP/%u.%u" (fun ma mi -> ma,mi)
        with
          _ -> failed Version version;
      in
      let meth = match meth with
      | "GET" -> `GET
      | "POST" -> `POST
      | "HEAD" -> `HEAD
      | _ -> failed Method meth
      in
      let rec loop hs s =
        match next s with
        | "",body ->
          let headers = List.rev_map (fun (n,v) -> String.lowercase n,v) hs in
          if version = (1,1) && not (List.mem_assoc "host" headers) then failed Header "Host is required for HTTP/1.1"; 
          let length = match Exn.catch (List.assoc "content-length") headers with
                       | None -> None
                       | Some s -> try Some (int_of_string s) with _ -> failed Header (sprintf "content-length %s" s)
          in
          begin match length, String.length body with
          | Some len, n when len <> n -> Exn.fail "not full body : %u <> %u" len n
          | None, n when n <> 0 -> failed Length "required"
          | _ ->
          let (path,args) = try String.split url "?" with _ -> url,"" in
          let decode_args s = try Netencoding.Url.dest_url_encoded_parameters s with _ -> log #debug "failed to parse args : %s" s; [] in
          let args = match meth with
          | `POST -> decode_args body
          | `GET | `HEAD -> decode_args args
          in
          {
            addr = addr;
            url = url;
            path = path;
            args = args;
            conn = conn;
            recv = Time.get ();
            headers = headers;
            body = body;
            meth = meth;
            version = version;
          }
          end
        | line,s ->
          let (n,v) = try String.split line ":" with _ -> failed Header line in
          loop ((n, String.strip v) :: hs) s
      in
      `Ok (loop [] s)
    | _ -> failed RequestLine line
  with
  exn -> `Error exn

let int_of_fd : Unix.file_descr -> int = Obj.magic

let close fd =
(*   Log.info "close %u" (int_of_fd fd); *)
  Exn.suppress (Unix.shutdown fd) Unix.SHUTDOWN_ALL;
  Unix.close fd

let write_f config status req (data,ack) ev fd _flags =
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
    match config.log_epipe, exn with
    | false, Unix.Unix_error (Unix.EPIPE,_,_) -> ()
    | _ -> log #warn ~exn "write_f %s" & Option.map_default show_request "" req

let http_reply = function
  | `Ok -> "HTTP/1.0 200 OK"

  | `Found -> "HTTP/1.0 302 Found"

  | `Bad_request -> "HTTP/1.0 400 Bad Request"
  | `Unauthorized -> "HTTP/1.0 401 Unauthorized"
  | `Forbidden -> "HTTP/1.0 403 Forbidden"
  | `Not_found -> "HTTP/1.0 404 Not Found"
  | `Length_required -> "HTTP/1.0 411 Length Required"
  | `Request_too_large -> "HTTP/1.0 413 Request Entity Too Large"

  | `Internal_server_error -> "HTTP/1.0 500 Internal Server Error"
  | `Not_implemented -> "HTTP/1.0 501 Not Implemented"
  | `Service_unavailable -> "HTTP/1.0 503 Service Unavailable"
  | `Version_not_supported -> "HTTP/1.0 505 HTTP Version Not Supported"

  | `Custom s -> s

(** Wait until [fd] becomes readable and close it (for eventfd-backed notifications) *)
let wait base fd k =
  Async.simple_event base fd [Ev.READ] begin fun ev fd _ ->
    Ev.del ev;
    Exn.suppress Unix.close fd;
    k ()
  end

let handle_client config status fd conn_info answer =
  let peer = Nix.show_addr (fst conn_info) in
  INC status.reqs;
  INC status.active;
  Unix.set_nonblock fd;
  let abort exn msg =
    (*Exn.suppress Ev.del ev;*)
    INC status.errs;
    DEC status.active;
    Exn.suppress close fd;
    log #warn ~exn "handle_client %s %s" msg peer
  in 
  let send_reply (req,(code,hdrs,body)) =
    try
    let b = Buffer.create 1024 in
    let put s = Buffer.add_string b s; Buffer.add_string b "\r\n" in
    put (http_reply code);
    List.iter (fun (n,v) -> bprintf b "%s: %s\r\n" n v) hdrs;
    bprintf b "Content-length: %u\r\n" (String.length body);
    put "Connection: close";
    put "";
    (* do not transfer body for HEAD requests *)
    let body = match req with Some x when x.meth = `HEAD -> "" | _ -> body in
    log #debug "will answer to %s with %d+%d bytes" 
      peer
      (Buffer.length b) 
      (String.length body);
(*     Buffer.add_string b body; *)
    Async.simple_event config.events fd [Ev.WRITE]
      (write_f config status req (ref [Buffer.contents b; body],ref 0))
    with
      exn -> abort exn "send"
  in
  log #debug "accepted %s" peer;
  Async.simple_event config.events fd [Ev.READ] begin fun ev fd _ ->
    try
    Ev.del ev; 
    let (req,x) = match Async.read_available ~limit:(256*1024) fd with
    | `Limit _ -> 
      log #info "read_all: request too large from %s" peer;
      None, `Now (`Request_too_large,[],"request entity too large")
(*
    | `Part s ->
      log #info "%s" s;
      log #warn "read_all: received partial request from %s" peer;
      None, `Now (`Bad_request,[],"")
*)
    (* FIXME may not read whole request *)
    | `Done data | `Part data ->
      log #debug "read_all: %d bytes from %s" (String.length data) peer;
      match parse_http_req data conn_info with
      | `Error (Parse (what,msg)) ->
        let error = match what with
        | Url | RequestLine | Header | Split | Version -> `Bad_request
        | Method -> `Not_implemented
        | Length -> `Length_required
        in
        log #warn "parse_http_req from %s : %s" peer msg;
        None, `Now (error,[],"")
      | `Error exn -> 
        log #warn ~exn "parse_http_req from %s" peer;
        None, `Now (`Bad_request,[],"")
      | `Ok req ->
        try
          let reply = 
            match req.version with
            | (1,_) -> answer status req
            | _ -> 
              log #info "version %u.%u not supported from %s" (fst req.version) (snd req.version) (show_request req);
              `Now (`Version_not_supported, [], "HTTP/1.0 is supported")
          in
          Some req, reply
        with exn ->
          log #error ~exn "answer %s" & show_request req;
          None, `Now (`Internal_server_error,[],"Internal server error")
    in
    match x with
    | `Now reply -> send_reply (req,reply)
    | `Later (fd,reply) -> wait config.events fd (fun () -> send_reply (req, !reply))
    with
    exn -> abort exn "send"
  end

include struct
open Unix

let start_listen config =
  let fd = socket PF_INET SOCK_STREAM 0 in
  set_nonblock fd;
  setsockopt fd SO_REUSEADDR true;
  let addr = ADDR_INET (config.ip,config.port) in
  bind fd addr;
  listen fd config.backlog;
  log #info "HTTP server ready on %s" (Nix.show_addr addr);
  fd

let setup_fd fd config answer = 
(*   open Unix in *)
  let status = { reqs = 0; active = 0; errs = 0; } in
  Async.simple_event config.events fd [Ev.READ] begin fun _ fd _ -> 
(*     Log.info "client";  *)
    try
      let (fd,addr) = accept fd in
      try
        handle_client config status fd (addr,Time.get()) answer
      with
        exn -> log #error ~exn "accepted (%s)" (Nix.show_addr addr)
    with
      _ -> () (* forked on single listening socket *)
  end

  let setup config answer =
    let fd = start_listen config in
    setup_fd fd config answer

end

let server config answer =
  setup config answer;
  Ev.dispatch config.events

let header n v = n,v
let forbidden = `Forbidden, [], "forbidden"
let not_found = `Not_found, [], "not found"
let found url = `Found,[header "Location" url], "found"

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

let serve (_req : request) ?status ?(extra=[]) ctype data =
  Option.default `Ok status, ("Content-Type",ctype) :: extra, data

let serve_io (req : request) ?status ?extra ctype (f : 'a IO.output -> unit) =
  serve req ?status ?extra ctype (output f)

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

let run ?(ip=Unix.inet_addr_loopback) port answer =
  server { default with ip = ip; port = port } answer

let input_header req name =
  List.assoc (String.lowercase name) req.headers

