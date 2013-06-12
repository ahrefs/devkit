(** Very simple and incomplete HTTP server *)

open Printf
open ExtLib
module Ev = Libevent

open Prelude

(* hide log *)
module Hidden = struct let log = Log.from "httpev" end
open Hidden

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
    name : string;
    max_request_size : int;
  }

let default = 
  {
    ip = Unix.inet_addr_loopback;
    port = 8080;
    backlog = 100;
    log_epipe = false;
    events = Ev.Global.base;
    name = "HTTP server";
    max_request_size = 16 * 1024;
  }

type request = { addr : Unix.sockaddr;
                 url : string; (* path and arguments *)
                 path : string;
                 args : (string * string) list;
                 conn : Time.t; (* time when client connected *)
                 recv : Time.t; (* time when client request was fully read *)
                 meth : [`GET | `POST | `HEAD ];
                 headers : (string * string) list;
                 body : string;
                 version : int * int; (* client HTTP version *)
                 id : int; (* request id *)
                 socket : Unix.file_descr;
                 line : string; (** request line *)
                 mutable blocking : unit IO.output option; (* hack for forked childs *)
                 }

(** server state *)
type server = {
  mutable total : int;
  mutable active : int;
  mutable errors : int;
  reqs : (int,request) Hashtbl.t;
  config : config;
}

type partial_body = {
  line1 : string;
  content_length : int option;
  parsed_headers : (string * string) list;
  buf : Buffer.t;
}

type request_state = Headers of Buffer.t | Body of partial_body | Ready of request

(** client state *)
type client = {
  fd : Unix.file_descr;
  req_id : int;
  time_conn : Time.t; (** time when client connected *)
  sockaddr : Unix.sockaddr;
  mutable req : request_state;
  server : server;
}

let show_method = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `HEAD -> "HEAD"

let show_client_addr req =
  try
    match req.addr with
    | Unix.ADDR_INET (addr,_) when addr = Unix.inet_addr_loopback ->
      let real = List.assoc "x-real-ip" req.headers in
      sprintf "%s(via:%s)" real (Nix.show_addr req.addr)
    | _ -> raise Not_found
  with Not_found ->
    Nix.show_addr req.addr

let client_addr req = match req.addr with Unix.ADDR_INET (addr,port) -> addr, port | _ -> assert false

let show_request req =
  sprintf "#%d %s time %.4f (recv %.4f) %s %s%s"
    req.id
    (show_client_addr req)
    (Time.get () -. req.conn)
    (req.recv -. req.conn)
    (show_method req.meth)
    (Exn.default "" (List.assoc "host") req.headers)
    req.url

let show_socket_error fd =
  try 
    match Unix.getsockopt_int fd Unix.SO_ERROR with
    | 0 -> ""
    | n -> sprintf ", socket error %d" n
  with _ -> ""

let show_peer c =
  sprintf "%s (%s%s)"
    (Nix.show_addr c.sockaddr)
    (Time.duration_str (Time.now () -. c.time_conn))
    (show_socket_error c.fd)

let show_client c =
  match c.req with
  | Headers b -> sprintf "%s headers %s" (show_peer c) (Action.bytes_string & Buffer.length b)
  | Body b -> sprintf "%s body %s" (show_peer c) (Action.bytes_string & Buffer.length b.buf)
  | Ready req -> show_request req

type ('a,'b) result = [ `Ok of 'a | `Error of 'b ]

let space = Pcre.regexp "[ \t]+"

type reason = Url | Version | Method | Header | RequestLine | Split | Extra
exception Parse of reason * string
let failed reason s =
  let name =
  match reason with
  | Url -> "url" | Version -> "version" | Method -> "method"
  | RequestLine -> "RequestLine" | Split -> "split" | Header -> "header"
  | Extra -> "Extra"
  in
  let lim = 1024 in
  let s =
    if String.length s > lim then
      sprintf "%S [%d bytes more]" (String.slice ~last:lim s) (String.length s - lim)
    else
      sprintf "%S" s
  in
  raise (Parse (reason, sprintf "%s : %s" name s))

let get_content_length headers =
  match Exn.catch (List.assoc "content-length") headers with
  | None -> None
  | Some s -> try Some (int_of_string s) with _ -> failed Header (sprintf "content-length %S" s)

let decode_args s =
  try Netencoding.Url.dest_url_encoded_parameters s with _ -> log #debug "failed to parse args : %s" s; []

let make_request c { line1; parsed_headers=headers; content_length; buf; } =
  match Pcre.split ~rex:space line1 with
  | [meth;url;version] ->
    if url.[0] <> '/' then (* abs_path *)
      failed Url url;
    let version = 
      try
        Scanf.sscanf version "HTTP/%u.%u" (fun ma mi -> ma,mi)
      with
        _ -> failed Version version
    in
    let meth = match meth with
    | "GET" -> `GET
    | "POST" -> `POST
    | "HEAD" -> `HEAD
    | _ -> failed Method meth
    in
    let (path,args) = try String.split url "?" with _ -> url,"" in
    let body = Buffer.contents buf in
    if version = (1,1) && not (List.mem_assoc "host" headers) then failed Header "Host is required for HTTP/1.1";
    let args = match meth with
    | `POST ->
      let args = decode_args args in
      let cont_type = try List.assoc "content-type" headers with _ -> "" in
      if cont_type = "application/x-www-form-urlencoded" then List.append args & decode_args body else args
    | `GET | `HEAD -> decode_args args
    in
    {
      url; path; args; headers; body; meth;
      id = c.req_id;
      addr = c.sockaddr;
      conn = c.time_conn;
      recv = Time.get ();
      version;
      blocking = None;
      line = line1;
      socket = c.fd;
    }
  | _ -> failed RequestLine line1

let extract_headers data =
  let open String in
  match nsplit data "\r\n" with
  | [] -> failed Split data
  | line1::xs -> 
    let headers = List.map (fun s ->
      try let (n,v) = split s ":" in lowercase (strip n), strip v with _ -> failed Header s) xs
    in
    line1, headers

let is_body_ready { line1; content_length; parsed_headers=_; buf; } final =
  match content_length, Buffer.contents buf with
  | None, "" -> true
  | None, body -> failed Extra body
  | Some length, body ->
    let body_len = String.length body in
    match body_len - length with
    | 0 -> true
    (* workaround MSIE 6 *)
    | 2 when String.starts_with line1 "POST" && body.[body_len - 2] = '\r' && body.[body_len - 1] = '\n' ->
      Buffer.clear buf;
      Buffer.add_string buf (String.slice ~last:(-2) body);
      true
    | n when final || n > 0 -> Exn.fail "wrong content-length : %d <> %d" length body_len
    | _ -> false

(* let int_of_fd : Unix.file_descr -> int = Obj.magic *)

let teardown fd =
(*   Log.info "close %u" (int_of_fd fd); *)
  Exn.suppress (Unix.shutdown fd) Unix.SHUTDOWN_ALL;
  Unix.close fd

let finish c =
  DEC c.server.active;
  teardown c.fd;
  match c.req with
  | Headers _ | Body _ -> ()
  | Ready req ->
    Hashtbl.remove c.server.reqs req.id;
    log #debug "finished %s" (show_request req)

let write_f c (data,ack) ev fd _flags =
  let finish () = finish c; Ev.del ev in
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
    INC c.server.errors;
    finish ();
    match c.server.config.log_epipe, exn with
    | false, Unix.Unix_error (Unix.EPIPE,_,_) -> ()
    | _ -> log #warn ~exn "write_f %s" (show_client c)

type reply_status = 
  [ `Ok
  | `Found
  | `Moved
  | `Bad_request
  | `Unauthorized
  | `Forbidden
  | `Not_found
  | `Length_required
  | `Request_too_large
  | `Internal_server_error
  | `Not_implemented
  | `Service_unavailable
  | `Version_not_supported
  | `No_reply
  | `Custom of string ]

type reply = reply_status * (string * string) list * string

exception No_reply

let status_code : reply_status -> int = function
  | `Ok -> 200

  | `Moved -> 301
  | `Found -> 302

  | `Bad_request -> 400
  | `Unauthorized -> 401
  | `Forbidden -> 403
  | `Not_found -> 404
  | `Length_required -> 411
  | `Request_too_large -> 413

  | `Internal_server_error -> 500
  | `Not_implemented -> 501
  | `Service_unavailable -> 503
  | `Version_not_supported -> 505

  | `Custom s -> 999

  | `No_reply -> 0

let http_reply_exn : reply_status -> string = function
  | `Ok -> "HTTP/1.0 200 OK"

  | `Moved -> "HTTP/1.0 301 Moved Permanently"
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

  | `No_reply -> raise No_reply

(** Wait until [fd] becomes readable and close it (for eventfd-backed notifications) *)
let wait base fd k =
  Async.simple_event base fd [Ev.READ] begin fun ev fd _ ->
    Ev.del ev;
    Exn.suppress Unix.close fd;
    k ()
  end

let write_some fd s =
  let slen = String.length s in
  if slen = 0 then `Done else
  try
    let len = Unix.write fd s 0 (String.length s) in
    if len = slen then `Done else `Some len
  with
  | Unix.Unix_error (Unix.EAGAIN,_,_) -> `Some 0

(** close transport connection, count as error *)
let abort c exn msg =
  INC c.server.errors;
  finish c;
  match c.server.config.log_epipe, exn with
  | false, Unix.Unix_error (Unix.EPIPE,_,_) -> ()
  | _ ->
    log #warn ~exn "abort %s %s" msg (show_client c)

let write_reply c l =
  let rec loop l =
    match l with
    | [] -> finish c
    | s::tl ->
      match write_some c.fd s with
      | `Some n -> Async.setup_simple_event c.server.config.events c.fd [Ev.WRITE] (write_f c (ref l,ref n))
      | `Done -> loop tl
  in
  try loop l with exn -> abort c exn "write_reply"

let write_reply_blocking c s =
  try
    let n = Unix.write c.fd s 0 (String.length s) in
    assert (n = String.length s)
  with
    exn -> abort c exn "write_reply_blocking"

let set_blocking req =
  let ch = Unix.out_channel_of_descr req.socket in
  let io = IO.output_channel ch in
  req.blocking <- Some io;
  io

let set_blocking' req =
  let ch = Unix.out_channel_of_descr req.socket in
  let io = IO.output_channel ch in
  req.blocking <- Some io;
  ch

let make_request_headers_exn code hdrs =
  let b = Buffer.create 1024 in
  let put s = Buffer.add_string b s; Buffer.add_string b "\r\n" in
  put (http_reply_exn code);
  List.iter (fun (n,v) -> bprintf b "%s: %s\r\n" n v) hdrs;
  put "Connection: close";
  put "";
  Buffer.contents b

let send_reply c (code,hdrs,body) =
  try
    let hdrs = ("Content-length", string_of_int (String.length body)) :: hdrs in
    let headers = make_request_headers_exn code hdrs in
    (* do not transfer body for HEAD requests *)
    let body = match c.req with Ready { meth = `HEAD; _ } -> "" | _ -> body in
    log #debug "will answer to %s with %d+%d bytes"
      (show_peer c)
      (String.length headers)
      (String.length body);
    write_reply c [headers;body]
  with
  | No_reply -> finish c
  | exn -> abort c exn "send_reply"

let send_reply_blocking c (code,hdrs) =
  try
    write_reply_blocking c (make_request_headers_exn code hdrs)
  with
  | No_reply -> finish c
  | exn -> abort c exn "send_reply_blocking"; raise exn

let send_reply_user c (code,hdrs,body) =
  match match c.req with Ready x -> x.blocking | _ -> None with
  | Some io ->
    Unix.clear_nonblock c.fd;
    send_reply_blocking c (code,hdrs);
  | None ->
    send_reply c (code,hdrs,body)

let send_error c exn =
  let (http_error,msg) = match exn with
  | Parse (what,msg) ->
    let error = match what with
    | Url | RequestLine | Header | Split | Version | Extra -> `Bad_request
    | Method -> `Not_implemented
    in
    error, msg
  | Failure s -> `Bad_request, s
  | exn -> `Bad_request, Exn.str exn
  in
  log #warn "error for %s : %s" (show_client c) msg;
  send_reply c (http_error,[],"")

let send_reply_limit c n =
  log #info "request too large from %s : %s" (show_client c) (Action.bytes_string n);
  send_reply c (`Request_too_large,[],"request entity too large")

let handle_request c body answer =
  let req = make_request c body in
  Hashtbl.replace c.server.reqs req.id req;
  c.req <- Ready req;
  try
    match req.version with
    | (1,_) -> answer c.server req (send_reply_user c)
    | _ ->
      log #info "version %u.%u not supported from %s" (fst req.version) (snd req.version) (show_request req);
      send_reply c (`Version_not_supported, [], "HTTP/1.0 is supported")
  with exn ->
    log #error ~exn "answer %s" & show_request req;
    match req.blocking with
    | None -> send_reply c (`Not_found,[],"Not found")
    | Some _ -> Exn.suppress teardown c.fd

let rec process_chunk c ev answer data final =
  match c.req with
  | Headers buf | Body { buf; _ } when String.length data + Buffer.length buf > c.server.config.max_request_size ->
    Ev.del ev;
    send_reply_limit c (String.length data + Buffer.length buf)
  | Headers buf ->
    Buffer.add_string  buf data;
    let input = Buffer.contents buf in
    begin match try Some (String.split input "\r\n\r\n") with _ -> None with
    | None -> if final then failed Split input (* continue reading headers *)
    | Some (headers,part) ->
      let (line1,headers) = extract_headers headers in
      let content_length = get_content_length headers in
      (** TODO transfer-encoding *)
      if List.mem_assoc "transfer-encoding" headers then Exn.fail "Transfer-Encoding not supported";
      Buffer.clear buf;
      let body = { line1; parsed_headers=headers; content_length; buf; } in
      c.req <- Body body;
      process_chunk c ev answer part final
    end
  | Body body ->
    Buffer.add_string body.buf data;
    if is_body_ready body final then
      (Ev.del ev; handle_request c body answer)
    else if final then
      failed Split (Buffer.contents body.buf)
  | Ready req ->
    if data = "" && final = true then
      (log #warn "STRANGE %s %B" (show_request req) final; failed Split "")
    else
      failed Extra data

let handle_client c answer =
  Async.setup_simple_event c.server.config.events c.fd [Ev.READ] begin fun ev fd _ ->
    try
      match Async.read_available ~limit:c.server.config.max_request_size fd with
      | `Limit s -> Ev.del ev; send_reply_limit c (String.length s)
      | `Chunk (data,final) -> process_chunk c ev answer data final
    with exn -> Ev.del ev; send_error c exn
  end

module Tcp = struct

open Unix

let listen ~name ?(backlog=100) addr port =
  let addr = ADDR_INET (addr,port) in
  let fd = socket PF_INET SOCK_STREAM 0 in
  try
    setsockopt fd SO_REUSEADDR true;
    bind fd addr;
    listen fd backlog;
    log #info "%s listen TCP %s" name (Nix.show_addr addr);
    fd
  with exn -> log #warn ~exn "%s listen TCP %s failed" name (Nix.show_addr addr); close fd; raise exn

let handle events fd k =
  set_nonblock fd;
  let rec setup () =
  Async.setup_simple_event events fd [Ev.READ] begin fun ev fd _ ->
    try
      while true do (* accept as much as possible, socket is nonblocking *)
        let peer = accept fd in
        try
          k peer
        with
          exn -> log #error ~exn "accepted (%s)" (Nix.show_addr (snd peer))
      done
    with
    | Unix_error(EAGAIN,_,_) -> ()
    | exn ->
(*
      log #error ~exn "accept (total requests %d)" (Hashtbl.length status.reqs);
      Hashtbl.iter (fun _ req -> log #error "%s" (show_request req)) status.reqs;
*)
      match exn with
      | Unix_error(EMFILE,_,_) ->
        let tm = 2. in
        log #error "disable listening socket for %s " (Time.duration_str tm);
        Ev.del ev; 
        let timer = Ev.create () in
        Ev.set_timer events timer ~persist:false (fun () ->
          Ev.del timer; log #info "reenabling listening socket"; setup ());
        Ev.add timer (Some tm)
      | _ -> ()
  end
  in
  setup ()

end

let start_listen config =
  Tcp.listen ~name:config.name ~backlog:config.backlog config.ip config.port

let setup_fd fd config answer =
  let server = { total = 0; active = 0; errors = 0; reqs = Hashtbl.create 10; config; } in
  Async.setup_periodic_timer_wait config.events (Time.minutes 1) begin fun () ->
    let now = Time.now () in
    server.reqs >> Hashtbl.iter begin fun _ req ->
      if req.recv -. now > Time.minutes 30 then
        log #warn "request takes too much time to process : %s" (show_request req)
    end
  end;
  Tcp.handle config.events fd (fun (fd,sockaddr) ->
    INC server.total;
    let req_id = server.total in
    INC server.active;
    let client = { fd; req_id; sockaddr; time_conn=Time.get (); server; req=Headers (Buffer.create 1024); } in
    Unix.set_nonblock fd;
    log #debug "accepted %s" (Nix.show_addr sockaddr);
    handle_client client answer)

let setup config answer =
  let fd = start_listen config in
  setup_fd fd config answer

let server config answer =
  setup config answer;
  Ev.dispatch config.events

let header n v = n,v
let forbidden = `Forbidden, [], "forbidden"
let not_found = `Not_found, [], "not found"
let found url = `Found,[header "Location" url], "found"
let moved url = `Moved,[header "Location" url], "moved permanently"
let cache_no = [header "Pragma" "no-cache"; header "Cache-Control" "max-age=0"]
let cache_yes t = [header "Last-Modified" (Time.to_rfc2822 t)]

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
  let get = Exn.catch arg
  let get_int = Exn.catch (int_of_string $ arg)
  let str name = match get name with Some s -> s | None -> raise (Bad name)
  let int name = let s = str name in try int_of_string s with _ -> raise (Bad name)
  (**
    @param name array name without brackets e.g. [array "x"] to extract [x] from /request?x[]=1&x[]=2
  *)
  let array name =
    let name = name ^ "[]" in
    T.req.args >> List.filter (fun (name',_) -> name = name') >> List.map snd
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

let find_header req name =
  List.assoc (String.lowercase name) req.headers

let header_exn req name =
  try find_header req name with _ -> Exn.fail "header %S" name

let header_safe req name = try find_header req name with _ -> ""

let header_referer req =
  try find_header req "Referer" with _ -> try find_header req "Referrer" with _ -> ""
