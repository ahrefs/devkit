(** Very simple and incomplete HTTP server *)

open Printf
open ExtLib
module Ev = Libevent

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

(* TODO put into request *)
type parsed_header =
{
  h_url : string;
  h_line : string;
  h_meth : [`GET | `POST | `HEAD ];
  h_headers : (string * string) list;
  h_version : int * int;
  h_start_body : int;
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
                 id : int; (* request id *)
                 fd : Unix.file_descr;
                 line : string; (** request line *)
                 packets : int; (* socket read number *)
                 mutable blocking : unit IO.output option; (* hack for forked childs *)
                 }

let show_method = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `HEAD -> "HEAD"

let show_client_addr c =
  try
    match c.addr with
    | Unix.ADDR_INET (addr,_) when addr = Unix.inet_addr_loopback ->
      let real = List.assoc "x-real-ip" c.headers in
      sprintf "%s(via:%s)" real (Nix.show_addr c.addr)
    | _ -> raise Not_found
  with Not_found ->
    Nix.show_addr c.addr

let client_addr c = match c.addr with Unix.ADDR_INET (addr,port) -> addr, port | _ -> assert false

let show_request c =
  sprintf "#%d %s time %.4f (recv %.4f) %s %s%s"
    c.id
    (show_client_addr c)
    (Time.get () -. c.conn)
    (c.recv -. c.conn)
    (show_method c.meth)
    (Exn.default "" (List.assoc "host") c.headers)
    c.url

type status = { mutable total : int; mutable active : int; mutable errs : int; reqs : (int,request) Hashtbl.t; }

type ('a,'b) result = [ `Ok of 'a | `Error of 'b ]

let space = Pcre.regexp "[ \t]+"

module ReqCache = struct
type t = (int ref) * Buffer.t * ((parsed_header option) ref)
let create () = (ref 0, Buffer.create 1024, ref None)
let add (i,b,_) s = incr i; Buffer.add_string b s
let count (i,_,_) = !i
let contents (_,b,_) = Buffer.contents b
let set_header (_,_,k) h = k := Some h
let get_header (_,_,h) = !h
end

type reason = Url | Version | Method | Header | RequestLine | Split | Length
exception Parse of reason * string
let failed reason s =
  let name =
  match reason with
  | Url -> "url" | Version -> "version" | Method -> "method"
  | RequestLine -> "RequestLine" | Split -> "split" | Header -> "header" | Length -> "length"
  in
  let lim = 1024 in
  let s = if String.length s > lim then sprintf "%S [%d bytes more]" (String.slice ~last:lim s) (String.length s - lim) else sprintf "%S" s in
  raise (Parse (reason, sprintf "%s : %s" name s))

let get_content_length headers =
  match Exn.catch (List.assoc "content-length") headers with
  | None -> 0
  | Some s -> try int_of_string s with _ -> failed Header (sprintf "content-length %S" s)
 
let parse_http_req req_id fd (addr,conn,buf) h =
  try
  let data = ReqCache.contents buf in
  let length = get_content_length h.h_headers in
  let body = try String.slice data ~first:h.h_start_body with _ -> failed Split data in
  let body_len = String.length body in
  let body = match body_len - length with
  | 0 -> body
  (* workaround MSIE 6 *)
  | 2 when h.h_meth = `POST && body.[body_len - 2] = '\r' && body.[body_len - 1] = '\n' -> String.slice ~last:(-2) body
  | _ -> Exn.fail "wrong content-length : %d <> %d" length body_len
  in
  let (path,args) = try String.split h.h_url "?" with _ -> h.h_url,"" in
  let decode_args s =
    try Netencoding.Url.dest_url_encoded_parameters s with _ -> log #debug "failed to parse args : %s" s; []
  in
  let args = match h.h_meth with
  | `POST -> decode_args body (* TODO check content-type *)
  | `GET | `HEAD -> decode_args args
  in
  `Ok {
    id = req_id;
    addr = addr;
    url = h.h_url;
    path = path;
    args = args;
    conn = conn;
    recv = Time.get ();
    headers = h.h_headers;
    body = body;
    meth = h.h_meth;
    version = h.h_version;
    blocking = None;
    line = h.h_line;
    fd = fd;
    packets = ReqCache.count buf;
  }
  with exn -> `Error exn

let parse_headers_exn data =
  let next s = try String.split s "\r\n" with _ -> failed Split data in
  let get_headers s =
    let rec loop acc s =
      match next s with
      | "",body ->
        List.rev_map (fun (n,v) -> String.(lowercase & strip n, strip v)) acc, body
      | line,s ->
        let header = try String.split line ":" with _ -> failed Header line in
        loop (header :: acc) s
    in
    loop [] s
  in
  let (line,extra) = next data in
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
    let (headers,_body) = get_headers extra in
    if version = (1,1) && not (List.mem_assoc "host" headers) then failed Header "Host is required for HTTP/1.1";
    {
      h_url = url;
      h_line = line;
      h_meth = meth;
      h_headers = headers;
      h_version = version;
      h_start_body = try String.find data "\r\n\r\n" + 4 with _ -> String.length data;
    }
  | _ -> failed RequestLine line

let manage_request_end_exn cache c =
  match c with
  | `Done _ ->
    if Option.is_none & ReqCache.get_header cache then
      ReqCache.set_header cache (parse_headers_exn (ReqCache.contents cache));
    true
  | `Part _ ->
    (* the first time header download finished, parse it! *)
    if (Option.is_none & ReqCache.get_header cache) && String.exists (ReqCache.contents cache) "\r\n\r\n" then
      ReqCache.set_header cache (parse_headers_exn (ReqCache.contents cache));
    (* check if we have the header, if we need body *)
    begin match ReqCache.get_header cache with
    | None -> false (* continue download header *)
    | Some h ->
      if h.h_meth <> `POST then true else begin
        let length = get_content_length h.h_headers in
        (String.length & ReqCache.contents cache) - h.h_start_body >= length
      end
    end

(* let int_of_fd : Unix.file_descr -> int = Obj.magic *)

let teardown fd =
(*   Log.info "close %u" (int_of_fd fd); *)
  Exn.suppress (Unix.shutdown fd) Unix.SHUTDOWN_ALL;
  Unix.close fd

let finish status fd req =
  DEC status.active;
  teardown fd;
  match req with
  | None -> ()
  | Some req ->
    Hashtbl.remove status.reqs req.id;
    log #debug "finished %s" (show_request req)

let write_f config status req (data,ack) ev fd _flags =
  let finish () = finish status fd req; Ev.del ev in
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
    INC status.errs;
    finish ();
    match config.log_epipe, exn with
    | false, Unix.Unix_error (Unix.EPIPE,_,_) -> ()
    | _ -> log #warn ~exn "write_f %s" & Option.map_default show_request "" req

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

let write_reply config fd status req l =
  let rec loop l =
    match l with
    | [] -> finish status fd req
    | s::tl ->
      match write_some fd s with
      | `Some n -> Async.setup_simple_event config.events fd [Ev.WRITE] (write_f config status req (ref l,ref n))
      | `Done -> loop tl
  in
  try
    loop l
  with
  | exn ->
    INC status.errs;
    finish status fd req;
    match config.log_epipe, exn with
    | false, Unix.Unix_error (Unix.EPIPE,_,_) -> ()
    | _ -> log #warn ~exn "write_reply %s" & Option.map_default show_request "" req

let write_reply_blocking_exn config fd status req s =
  try
    let n = Unix.write fd s 0 (String.length s) in
    assert (n = String.length s)
  with
  | exn ->
    INC status.errs;
    finish status fd req;
    begin match config.log_epipe, exn with
    | false, Unix.Unix_error (Unix.EPIPE,_,_) -> ()
    | _ -> log #warn ~exn "write_reply_blocking %s" & Option.map_default show_request "" req
    end;
    raise exn

let set_blocking req =
  let ch = Unix.out_channel_of_descr req.fd in
  let io = IO.output_channel ch in
  req.blocking <- Some io;
  io

let set_blocking' req =
  let ch = Unix.out_channel_of_descr req.fd in
  let io = IO.output_channel ch in
  req.blocking <- Some io;
  ch

let show_socket_error fd =
  try 
    match Unix.getsockopt_int fd Unix.SO_ERROR with
    | 0 -> ""
    | n -> sprintf ", socket error %d" n
  with _ -> ""

let make_request_headers_exn code hdrs =
  let b = Buffer.create 1024 in
  let put s = Buffer.add_string b s; Buffer.add_string b "\r\n" in
  put (http_reply_exn code);
  List.iter (fun (n,v) -> bprintf b "%s: %s\r\n" n v) hdrs;
  put "Connection: close";
  put "";
  Buffer.contents b

let handle_client config status fd ((peer,req_start,cache) as conn_info) answer =
  let peer = Nix.show_addr peer in
  let show_peer () =
    sprintf "%s (%s%s)" peer (Time.duration_str (Time.now () -. req_start)) (show_socket_error fd)
  in
  let abort req exn msg =
    (*Exn.suppress Ev.del ev;*)
    INC status.errs;
    finish status fd req;
    log #warn ~exn "handle_client %s %s" msg (show_peer ())
  in 
  let send_reply req (code,hdrs,body) =
    try
      let hdrs = ("Content-length", string_of_int (String.length body)) :: hdrs in
      let headers = make_request_headers_exn code hdrs in
      (* do not transfer body for HEAD requests *)
      let body = match req with Some x when x.meth = `HEAD -> "" | _ -> body in
      log #debug "will answer to %s with %d+%d bytes"
        (show_peer ())
        (String.length headers)
        (String.length body);
      write_reply config fd status req [headers;body]
    with
    | No_reply -> finish status fd req
    | exn -> abort req exn "send_reply"
  in
  let send_reply_blocking req (code,hdrs) =
    try
      let content = make_request_headers_exn code hdrs in
      write_reply_blocking_exn config fd status req content
    with
    | No_reply -> finish status fd req
    | exn -> abort req exn "send_reply"; raise exn
  in
  let send_reply_user req (code,hdrs,body) =
    match match req with Some x -> x.blocking | None -> None with
    | Some io ->
      Unix.clear_nonblock fd;
      send_reply_blocking req (code,hdrs);
    | None ->
      send_reply req (code,hdrs,body)
  in
  let send_error exn =
    let (http_error,msg) = match exn with
    | Parse (what,msg) ->
      let error = match what with
      | Url | RequestLine | Header | Split | Version -> `Bad_request
      | Method -> `Not_implemented
      | Length -> `Length_required
      in
      error, msg
    | exn ->
      `Bad_request, Exn.str exn
    in
    log #warn "parse_http_req from %s, got %d bytes : %s" (show_peer ()) (String.length & ReqCache.contents cache) msg;
    send_reply None (http_error,[],"")
  in

  INC status.total;
  let req_id = status.total in
  INC status.active;
  Unix.set_nonblock fd;
  log #debug "accepted %s" peer;
  Async.setup_simple_event config.events fd [Ev.READ] begin fun ev fd _ ->
    try
    match Async.read_available ~limit:(256*1024) fd with
    | `Limit _ -> 
      Ev.del ev;
      log #info "read_all: request too large from %s" (show_peer ());
      send_reply None (`Request_too_large,[],"request entity too large")
    | `Done data | `Part data as c -> 
      ReqCache.add cache data;
      try
        match data with
        | "" -> (* special case for better error message *)
          Ev.del ev;
          log #warn "parse_http_req from %s : client disconnected without sending anything" (show_peer ());
          finish status fd None
        | _ when manage_request_end_exn cache c ->
        Ev.del ev;
        log #debug "done %d queries, cache length is %d" (ReqCache.count cache) (String.length & ReqCache.contents cache);
        begin
        match parse_http_req req_id fd conn_info (Option.get & ReqCache.get_header cache) with
        | `Error exn -> send_error exn
        | `Ok req ->
          try
            Hashtbl.replace status.reqs req.id req;
            match req.version with
            | (1,_) -> answer status req (send_reply_user (Some req))
            | _ ->
              log #info "version %u.%u not supported from %s" (fst req.version) (snd req.version) (show_request req);
              send_reply (Some req) (`Version_not_supported, [], "HTTP/1.0 is supported")
          with exn ->
            log #error ~exn "answer %s" & show_request req;
            match req.blocking with
            | None -> send_reply (Some req) (`Not_found,[],"Not found")
            | Some _ -> Exn.suppress teardown fd
        end
        | _ -> () (* continute reading to buffer *)
        with exn -> Ev.del ev; send_error exn
    with
    exn -> abort None exn "send"
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
  let status = { total = 0; active = 0; errs = 0; reqs = Hashtbl.create 10; } in
  let rec setup () =
  Async.setup_simple_event events fd [Ev.READ] begin fun ev fd _ ->
    try
      while true do (* accept as much as possible, socket is nonblocking *)
        let peer = accept fd in
        try
          k status peer
        with
          exn -> log #error ~exn "accepted (%s)" (Nix.show_addr (snd peer))
      done
    with
    | Unix_error(EAGAIN,_,_) -> ()
    | exn ->
      log #error ~exn "accept (total requests %d)" (Hashtbl.length status.reqs);
      Hashtbl.iter (fun _ req -> log #error "%s" (show_request req)) status.reqs;
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
  Tcp.listen ~name:"HTTP server" ~backlog:config.backlog config.ip config.port

let setup_fd fd config answer =
  Tcp.handle config.events fd (fun st (fd,addr) ->
    handle_client config st fd (addr,Time.get(),ReqCache.create()) answer)

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
