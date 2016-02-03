(** Very simple and incomplete HTTP server *)

open Printf
open ExtLib
module Ev = Libevent

open Prelude

let buffer_size = 4096

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
    mutable debug : bool; (** more logging *)
    events : Ev.event_base;
    access_log : out_channel ref;
    name : string;
    max_request_size : int;
    auth : (string * string * string) option;
    max_clients : int; (** limit on total number of requests in processing at any point of time *)
    max_data_childs : int;
    max_data_waiting : int;
    yield : bool;
      (** do [Lwt_unix.yield ()] after accepting connection to give other lwt threads chance to run (set to [true] when http requests
          processing causes other threads to stuck) *)
  }

let default =
  {
    ip = Unix.inet_addr_loopback;
    port = 8080;
    backlog = 100;
    log_epipe = false;
    debug = false;
    events = Ev.Global.base;
    name = "HTTP server";
    max_request_size = 16 * 1024;
    auth = None;
    max_clients = 10_000;
    access_log = ref stdout;
    max_data_childs = 50;
    max_data_waiting = 200;
    yield = true;
  }

include Httpev_common

(** server state *)
type server = {
  listen_socket : Unix.file_descr;
  mutable total : int;
  mutable active : int;
  mutable errors : int;
  mutable reject : int;
  reqs : (int,request) Hashtbl.t;
  config : config;
  digest_auth : Digest_auth.t option;
  h_childs : (int,reply -> unit) Hashtbl.t; (** currently running forked childs *)
  q_wait : (unit -> unit) Stack.t; (** the stack of requests to fork *)
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

let make_server_state fd config =
  let digest_auth =
    match config.auth with
    | Some (realm,user,password) -> Some (Digest_auth.init ~realm ~user ~password ())
    | None -> None
  in
  {
    total = 0;
    active = 0;
    errors = 0;
    reject = 0;
    reqs = Hashtbl.create 10;
    config;
    digest_auth;
    listen_socket = fd;
    h_childs = Hashtbl.create 16;
    q_wait = Stack.create ();
  }

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
  | Headers b -> sprintf "%s headers %s" (show_peer c) (Action.bytes_string @@ Buffer.length b)
  | Body b -> sprintf "%s body %s" (show_peer c) (Action.bytes_string @@ Buffer.length b.buf)
  | Ready req -> show_request req

type ('a,'b) result = [ `Ok of 'a | `Error of 'b ]

let space = Pcre.regexp "[ \t]+"

type reason = Url | Version | Method | Header | RequestLine | Split | Extra | NotAcceptable
exception Parse of reason * string
let failed reason s =
  let name =
  match reason with
  | Url -> "url" | Version -> "version" | Method -> "method"
  | RequestLine -> "RequestLine" | Split -> "split" | Header -> "header"
  | Extra -> "Extra"
  | NotAcceptable -> "Not Acceptable"
  in
  let s = Stre.shorten 1024 s in
  raise (Parse (reason, sprintf "%s : %s" name s))

let get_content_length headers =
  match Exn.catch (List.assoc "content-length") headers with
  | None -> None
  | Some s -> try Some (int_of_string s) with _ -> failed Header (sprintf "content-length %S" s)

let decode_args s =
  try Netencoding.Url.dest_url_encoded_parameters s with exn -> Exn.fail ~exn "decode_args : %S" s

(** Minimum strictness, Neturl will fail on malformed parameters in url *)
let decode_args_soft s =
  try
    String.nsplit s "&" |>
    List.filter_map (fun s -> try String.split s "=" |> apply2 Web.urldecode |> some with _ -> None)
  with
    exn -> Exn.fail ~exn "decode_args_soft : %S" s

let acceptable_encoding headers =
  let split s c = List.map (String.strip ~chars:" \t\r\n") @@ Stre.nsplitc s c in
  match Exn.catch (List.assoc "accept-encoding") headers with
  | Some str ->
    let encodings = split str ',' |> List.filter_map begin fun s ->
      match split (String.lowercase s) ';' with
      | [ enc ] -> Some (enc, None)
      | [ enc; q ] -> Some (enc, Some q)
      | _ -> log #warn "bad accept-encoding record, ignoring : %S" s; None
    end in
    let test_available s =
      if List.mem (s, Some "q=0") encodings then false else
      if List.exists (fun (enc,_) -> enc = s) encodings then true else
      if List.mem ("*", Some "q=0") encodings then false else
      if List.exists (fun (enc,_) -> enc = "*") encodings then true else
      s = "identity" (* identity is always accepted, unless prohibited *)
    in
    if test_available "gzip" then Gzip else
    if test_available "identity" then Identity
    else Exn.fail "not acceptable : %S" str
  | None -> Identity

let make_request_exn c { line1; parsed_headers=headers; buf; _ } =
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
      if cont_type = "application/x-www-form-urlencoded" then List.append args @@ decode_args body else args
    | `GET | `HEAD -> decode_args args
    in
    let encoding = try acceptable_encoding headers with Failure s -> failed NotAcceptable s in
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
      encoding;
    }
  | _ -> failed RequestLine line1

let extract_header s =
  let open String in
  try let (n,v) = split s ":" in lowercase (strip n), strip v with _ -> failed Header s

let extract_headers data =
  match String.nsplit data "\r\n" with
  | [] -> failed Split data
  | line1::xs -> line1, List.map extract_header xs

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
    if c.server.config.debug then
      log #info "finished %s" (show_request req)

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

let find_header req name =
  List.assoc (String.lowercase name) req.headers

let header_exn req name =
  try find_header req name with _ -> Exn.fail "header %S" name

let header_safe req name = try find_header req name with _ -> ""

let header_referer req =
  try find_header req "Referer" with _ -> try find_header req "Referrer" with _ -> ""

let log_access_apache ch code size req =
  try
    let now = Time.now () in
    fprintf ch "%s - - [%s] %S %d %dB . %S %S %.3f %s\n%!"
      (show_client_addr req) (Time.to_string now) req.line code size
      (header_referer req) (header_safe req "user-agent") (now -. req.conn) (header_safe req "host")
  with exn ->
    log #warn ~exn "access log : %s" @@ show_request req

let log_status_apache ch status size req =
  match status with
  | `No_reply -> () (* ignore *)
  | #reply_status as code -> log_access_apache ch (status_code code) size req

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
  let encode = match req.encoding with Identity -> id | Gzip -> Gzip_io.output in
  let io = encode @@ IO.output_channel ch in
  req.blocking <- Some io;
  io

let make_request_headers code hdrs =
  let b = Buffer.create 1024 in
  let put s = Buffer.add_string b s; Buffer.add_string b "\r\n" in
  put (show_http_reply code);
  List.iter (fun (n,v) -> bprintf b "%s: %s\r\n" n v) hdrs;
  put "Connection: close";
  put "";
  Buffer.contents b

let send_reply_async c encoding (code,hdrs,body) =
  try
    (* possibly apply encoding *)
    let (hdrs,body) =
      (* TODO do not apply encoding to application/gzip *)
      match encoding with
      | Gzip when String.length body > 128 -> ("Content-Encoding", "gzip") :: hdrs, Gzip_io.string body
      | _ -> hdrs, body
    in
    let hdrs = ("Content-Length", string_of_int (String.length body)) :: hdrs in
    (* do not transfer body for HEAD requests *)
    let body = match c.req with Ready { meth = `HEAD; _ } -> "" | _ -> body in
    let headers = make_request_headers code hdrs in
    if c.server.config.debug then
      log #info "will answer to %s with %d+%d bytes"
        (show_peer c)
        (String.length headers)
        (String.length body);
    write_reply c [headers;body]
  with
  | exn -> abort c exn "send_reply_async"

let send_reply_blocking c (code,hdrs) =
  try
    write_reply_blocking c @@ make_request_headers code hdrs
  with
    exn -> abort c exn "send_reply_blocking"; raise exn

(* this function is called back by user to actually send data *)
let send_reply_user c req (code,hdrs,body) =
  match code with
  | `No_reply -> finish c
  | #reply_status as code ->
  let hdrs =
    match hdrs with
    (* hack for answer_forked, which logs on its own *)
    | ("X-Disable-Log", "true") :: hs -> hs
    | _ ->
      log_status_apache !(c.server.config.access_log) code (String.length body) req;
      hdrs
  in
  let blocking = Option.is_some req.blocking in
  (* filter headers *)
  let hdrs = hdrs |> List.filter begin fun (k,_) ->
    let open Stre in
    let forbidden =
      (iequal k "content-length" && not blocking) || (* httpev will calculate *)
      (iequal k "connection") ||
      (iequal k "content-encoding") (* none of the user's business *)
    in
    not forbidden
  end in
  match blocking with
  | true ->
    (* this is forked child, events are gone, so write to socket with blocking *)
    Unix.clear_nonblock c.fd;
    let hdrs = match req.encoding with Identity -> hdrs | Gzip -> ("Content-Encoding", "gzip") :: hdrs in
    send_reply_blocking c (code,hdrs);
  | false ->
    send_reply_async c req.encoding (code,hdrs,body)

let make_error = function
| Parse (what,msg) ->
  let error = match what with
  | Url | RequestLine | Header | Split | Version | Extra -> `Bad_request
  | Method -> `Not_implemented
  | NotAcceptable -> `Not_acceptable
  in
  error, msg
| Failure s -> `Bad_request, s
| exn -> `Bad_request, Exn.str exn

let send_reply_limit c n =
  log #info "request too large from %s : %s" (show_client c) (Action.bytes_string n);
  send_reply_async c Identity (`Request_too_large,[],"request entity too large")

let handle_request c body answer =
  let req = make_request_exn c body in
  Hashtbl.replace c.server.reqs req.id req;
  c.req <- Ready req;
  try
    match req.version with
    | (1,_) ->
      let auth = match c.server.digest_auth with
      | Some auth -> Digest_auth.check auth req
      | None -> `Ok
      in
      let k = send_reply_user c req in
      begin match auth with
      | `Unauthorized header -> k (`Unauthorized, [header], "Unauthorized")
      | `Ok -> answer c.server req k
      end
    | _ ->
      log #info "version %u.%u not supported from %s" (fst req.version) (snd req.version) (show_request req);
      send_reply_async c Identity (`Version_not_supported,[],"HTTP/1.0 is supported")
  with exn ->
    log #error ~exn "answer %s" @@ show_request req;
    match req.blocking with
    | None -> send_reply_async c Identity (`Not_found,[],"Not found")
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
    with exn ->
      Ev.del ev;
      let (http_error,msg) = make_error exn in
      log #warn "error for %s : %s" (show_client c) msg;
      send_reply_async c Identity (http_error,[],"")
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

let check_hung_requests server =
  let now = Time.now () in
  server.reqs |> Hashtbl.iter begin fun _ req ->
    if req.recv -. now > Time.minutes 30 then
      log #warn "request takes too much time to process : %s" (show_request req)
  end

let check_waiting_requests srv =
  while not (Stack.is_empty srv.q_wait) && Hashtbl.length srv.h_childs < srv.config.max_data_childs do
    let f = Stack.pop srv.q_wait in
    begin try let () = f () in () with exn -> log #warn ~exn "q_wait" end
  done

let finish_child srv pid =
(*     log #info "child %d reaped" pid; *)
    match Hashtbl.find_option srv.h_childs pid with
    | Some k ->
      Hashtbl.remove srv.h_childs pid;
      k (`No_reply,[],""); (* just close socket *)
      check_waiting_requests srv
    | None -> log #warn "no handler for child %d" pid

let reap_orphans srv =
  let rec loop () =
    match Exn.catch (Unix.waitpid [Unix.WNOHANG]) 0 with
    | None | Some (0,_) -> ()
    | Some (pid,st) -> log #info "reaped orphan %d %S" pid (Std.dump st); finish_child srv pid; loop ()
  in loop ()

let start_listen config =
  Tcp.listen ~name:config.name ~backlog:config.backlog config.ip config.port

let setup_server_fd fd config answer =
  let server = make_server_state fd config in
  Async.setup_periodic_timer_wait config.events (Time.minutes 1) (fun () -> check_hung_requests server);
  Async.setup_periodic_timer_now config.events 10. (fun () -> reap_orphans server);
  Tcp.handle config.events fd begin fun (fd,sockaddr) ->
    INC server.total;
    let req_id = server.total in
    match server.active >= config.max_clients with
    | true ->
      INC server.reject;
      if config.debug then log #info "rejected #%d %s" req_id (Nix.show_addr sockaddr);
      teardown fd
    | false ->
      INC server.active;
      let client = { fd; req_id; sockaddr; time_conn=Time.get (); server; req=Headers (Buffer.create 1024); } in
      Unix.set_nonblock fd;
      if config.debug then log #info "accepted #%d %s" req_id (Nix.show_addr sockaddr);
      handle_client client answer
  end;
  server

let setup_server config answer =
  let fd = start_listen config in
  setup_server_fd fd config answer

let setup_fd fd config answer = let (_:server) = setup_server_fd fd config answer in ()
let setup config answer = let (_:server) = setup_server config answer in ()

let run config answer =
  setup config answer;
  Ev.dispatch config.events

let server = run (* deprecated *)

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

(** {2 Utilities} *)

module Args(T : sig val req : request end) : sig
  exception Bad of string
  val get : string -> string option
  (** Get optional parameter. @return None if parameter is missing *)
  val str : ?default:string -> string -> string
  (** Get required parameter. @raise Bad if parameter is missing and no [default] provided *)
  val get_int : string -> int option
  (** Get optional integer parameter *)
  val int : ?default:int -> string -> int
  (** Get integer parameter. @raise Bad if parameter is missing and no [default] provided *)
  val float : ?default:float -> string -> float
  val int64 : ?default:int64 -> string -> int64
  val array : string -> string list
  (** @param name array name without brackets e.g. [array "x"] to extract [x] from /request?x[]=1&x[]=2 *)
end =
struct
  let arg name = List.assoc name T.req.args
  exception Bad of string
  let get = Exn.catch arg
  let get_int = Exn.catch (int_of_string $ arg)
  let make f ?default name =
    match get name, default with
    | None, None -> raise (Bad name)
    | None, Some s -> s
    | Some s, _ -> try f s with _ -> raise (Bad name)
  let str = make id
  let int64 = make Int64.of_string
  let int = make int_of_string
  let float = make float_of_string
  let array name =
    let name = name ^ "[]" in
    T.req.args |> List.filter (fun (name',_) -> name = name') |> List.map snd
end

let noclose_io io =
  IO.create_out
    ~write:(IO.write io)
    ~output:(IO.output io)
    ~flush:(fun () -> IO.flush io)
    ~close:(fun () -> ())

(** Buffers all output *)
let output (f : 'a IO.output -> unit) =
  let out = IO.output_string () in
  f @@ noclose_io out;
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

let run ?(ip=Unix.inet_addr_loopback) port answer =
  server { default with ip = ip; port = port } answer

(** {2 Forked workers} *)

let answer_forked ?(debug=false) srv req answer k =
  let do_fork () =
    let check () = match Unix.getsockopt_int req.socket Unix.SO_ERROR with
    | 0 -> `Ok
    | n -> `Error n
    in
    match check () with
    | `Error n -> Exn.fail "pre fork %s : socket error %d" (show_request req) n
    | `Ok ->
    begin match Lwt_unix.fork () with
    | 0 ->
      Exn.suppress Unix.close srv.listen_socket;
      let count = ref 0L in
      let code = try
      if debug then Printexc.record_backtrace true;
      Control.with_output (set_blocking req) begin fun io ->
        let check_exn () =
          match check () with `Ok -> () | `Error n -> Exn.fail "socket error %d" n
        in
        let io = Action.count_bytes_to count io in
        let pre h =
          k (`Ok, ("X-Disable-Log", "true") :: h,"");
          check_exn ()
        in
        check_exn ();
        let () = answer pre io in
        200
      end
      with exn ->
        let saved_backtrace = Exn.get_backtrace () in
        log #warn ~exn ~backtrace:debug ~saved_backtrace "answer forked %s" (show_request req);
        -1
      in
      log_access_apache !(srv.config.access_log) code (Int64.to_int !count) req;
      U.sys_exit 0
    | -1 -> Exn.fail "fork failed : %s" (show_request req)
    | pid ->
      log #info "forked %d : %s" pid (show_request req);
      Hashtbl.add srv.h_childs pid k
    end
  in
  let do_fork () =
    try
      do_fork ()
    with
      exn ->
        log #warn ~exn "answer fork failed %s" (show_request req);
        k (`Internal_server_error,[],"")
  in
  if Hashtbl.length srv.h_childs < srv.config.max_data_childs then
    do_fork ()
  else
  if Stack.length srv.q_wait < srv.config.max_data_waiting then
    Stack.push do_fork srv.q_wait
  else
  begin
    log #info "rejecting, overloaded : %s" (show_request req);
    k (`Service_unavailable, ["Content-Type", "text/plain"], "overloaded")
  end

(** {2 Lwt support} *)

let default_timeout = 30.
let timeout thread = Lwt_unix.with_timeout default_timeout (fun () -> thread)

let send_reply c cout reply =
  (* repack *)
  let (code,hdrs,body) = match reply with
  | `Body (code,hdrs,s) -> code, hdrs, `Body s
  | `Chunks (code,hdrs,gen) -> code, hdrs, `Chunks gen
  in
  begin match c.req with
  | Ready req -> log_status_apache !(c.server.config.access_log) code (match body with `Body s -> String.length s | `Chunks _ -> 0) req
  | _ -> () (* this can happen when sending back error reply on malformed HTTP input *)
  end;
  (* filter headers *)
  let hdrs = hdrs |> List.filter begin fun (k,_) ->
    let open Stre in
    let forbidden =
      (iequal k "content-length") || (* httpev will calculate *)
      (iequal k "connection") ||
      (iequal k "transfer-encoding") ||
      (iequal k "content-encoding") (* none of the user's business *)
    in
    not forbidden
  end
  in
  (* possibly apply encoding *)
  let (hdrs,body) =
    (* TODO do not apply encoding to application/gzip *)
    (* TODO gzip + chunked? *)
    match body, code, c.req with
    | `Body s, `Ok, Ready { encoding=Gzip; _ } when String.length s > 128 -> ("Content-Encoding", "gzip")::hdrs, `Body (Gzip_io.string s)
    | _ -> hdrs, body
  in
  let hdrs = match body with
  | `Body s -> ("Content-Length", string_of_int (String.length s)) :: hdrs
  | `Chunks _ -> ("Transfer-Encoding", "chunked") :: hdrs
  in
  (* do not transfer body for HEAD requests *)
  let body = match c.req with Ready { meth = `HEAD; _ } -> `Body "" | _ -> body in
  let headers = make_request_headers code hdrs in
  if c.server.config.debug then
    log #info "will answer to %s with %d+%s bytes"
      (show_peer c)
      (String.length headers)
      (match body with `Body s -> sprintf "%d" (String.length s) | `Chunks _ -> "...");
  lwt () = Lwt_io.write cout headers in
  lwt () =
    match body with
    | `Body s -> Lwt_io.write cout s
    | `Chunks gen ->
      let push = function
      | "" -> Lwt.return ()
      | s ->
        lwt () = Lwt_io.write cout (sprintf "%x\r\n" (String.length s)) in
        lwt () = Lwt_io.write cout s in
        Lwt_io.write cout "\r\n"
      in
      try_lwt
        lwt () = gen push in
        Lwt_io.write cout "0\r\n\r\n"
      with exn ->
        (* do not write trailer on error - let the peer notice the breakage *)
        log #warn ~exn "generate failed";
        Lwt.return ()
  in
  Lwt_io.flush cout

let handle_request_lwt c req answer =
  let return x = Lwt.return @@ `Body x in
  match req.version with
  | (1,_) ->
    let auth = match c.server.digest_auth with
    | Some auth -> Digest_auth.check auth req
    | None -> `Ok
    in
    begin match auth with
    | `Unauthorized header -> return (`Unauthorized, [header], "Unauthorized")
    | `Ok ->
      try_lwt
        answer c.server req
      with exn ->
        log #error ~exn "answer %s" @@ show_request req;
        return (`Not_found,[],"Not found")
    end
  | _ ->
    log #info "version %u.%u not supported from %s" (fst req.version) (snd req.version) (show_request req);
    return (`Version_not_supported,[],"HTTP/1.0 is supported")

let handle_client_lwt client cin answer =
  (* TODO client.server.config.max_request_size *)
  let rec read_headers acc =
    match_lwt Lwt_io.read_line cin with
    | "" -> Lwt.return acc
    | s -> read_headers (extract_header s :: acc)
  in
  lwt line1 = Lwt_io.read_line cin in
  lwt headers = read_headers [] in
  let content_length = get_content_length headers in
  (** TODO transfer-encoding *)
  if List.mem_assoc "transfer-encoding" headers then Exn.fail "Transfer-Encoding not supported";
  lwt data =
    match content_length with
    | None -> Lwt.return ""
    | Some n ->
      let s = Bytes.create n in
      lwt () = Lwt_io.read_into_exactly cin s 0 n in
      Lwt.return s
  in
  (* TODO check that no extra bytes arrive *)
  let body = { line1; parsed_headers=headers; content_length; buf = Buffer.create (String.length data); } in
  client.req <- Body body;
  Buffer.add_string body.buf data;
  let req = make_request_exn client body in
  client.req <- Ready req;
  Hashtbl.replace client.server.reqs req.id req;
  try_lwt
    handle_request_lwt client req answer
  finally
    Hashtbl.remove client.server.reqs req.id;
    Lwt.return ()

let accept_hook = ref (fun () -> ())

let handle_lwt fd k =
  !accept_hook ();
  match_lwt Exn_lwt.map Lwt_unix.accept fd with
  | `Exn (Unix.Unix_error (Unix.EMFILE,_,_)) ->
    let pause = 2. in
    log #error "too many open files, disabling accept for %s" (Time.duration_str pause);
    Lwt_unix.sleep pause
  | `Exn exn -> log #warn ~exn "accept"; Lwt.return ()
  | `Ok (fd,addr as peer) ->
    let task =
      try_lwt k peer
      with exn -> log #warn ~exn "accepted (%s)" (Nix.show_addr addr); Lwt.return ()
      finally
        Lwt_unix.(Exn.suppress (shutdown fd) SHUTDOWN_ALL);
        Lwt_unix.close fd
    in
    Lwt.ignore_result task; (* "fork" processing *)
    Lwt.return ()

let handle_lwt config fd k =
  let rec loop () =
    lwt () = handle_lwt fd k in
    lwt () = if config.yield then Lwt_unix.yield () else Lwt.return_unit in
    loop ()
  in
  lwt () = Lwt.choose [Daemon.should_exit_lwt; loop ()] in
  log #info "%s %s:%d exit" config.name (Unix.string_of_inet_addr config.ip) config.port;
  Lwt.return_unit

module BuffersCache = Cache.Reuse(struct type t = Lwt_bytes.t let create () = Lwt_bytes.create buffer_size let reset = ignore end)

let setup_fd_lwt fd config answer =
  let server = make_server_state (Lwt_unix.unix_file_descr fd (* will not be used *) ) config in
  Lwt.ignore_result begin
    while_lwt true do
      lwt () = Lwt_unix.sleep @@ Time.minutes 1 in
      Lwt.wrap1 check_hung_requests server
    done
  end;
  handle_lwt config fd begin fun (fd,sockaddr) ->
    INC server.total;
    let req_id = server.total in
    match server.active >= config.max_clients with
    | true -> INC server.reject; if config.debug then log #info "rejected #%d %s" req_id (Nix.show_addr sockaddr); Lwt.return_unit
    | false ->
    INC server.active;
    let error = lazy (INC server.errors) in
    let client =
      (* used only in show_socket_error *)
      { fd = Lwt_unix.unix_file_descr fd; req_id; sockaddr; time_conn=Time.get (); server; req=Headers (Buffer.create 0); }
    in
    if config.debug then log #info "accepted #%d %s" req_id (Nix.show_addr sockaddr);
    let buffer = BuffersCache.get () in
    let cin = Lwt_io.(of_fd ~buffer ~close:Lwt.return ~mode:input fd) in
    lwt reply =
      try_lwt
        handle_client_lwt client cin answer
      with exn ->
        !!error;
        let (http_error,msg) = make_error exn in
        log #warn "error for %s : %s" (show_client client) msg;
        Lwt.return @@ `Body (http_error,[],"")
    in
    (* reusing same buffer! *)
    let cout = Lwt_io.(of_fd ~buffer ~close:Lwt.return ~mode:output fd) in
    try_lwt
      send_reply client cout reply
    with exn ->
      !!error;
      log #warn ~exn "send_reply %s" (show_client client);
      Lwt.return ()
    finally
      DEC server.active;
      BuffersCache.release buffer;
      Lwt.return ()
  end

let setup_lwt config answer =
  let fd = Lwt_unix.of_unix_file_descr ~blocking:false @@ start_listen config in
  setup_fd_lwt fd config answer

let server_lwt config answer =
  Lwt_main.run @@ setup_lwt config answer
