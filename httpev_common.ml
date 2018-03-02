open Printf

type encoding = Gzip | Identity

type request = { addr : Unix.sockaddr;
                 url : string; (* path and arguments *)
                 path : string;
                 args : (string * string) list;
                 conn : Time.t; (* time when client connected *)
                 recv : Time.t; (* time when client request was fully read *)
                 meth : [`GET | `POST | `PUT | `PATCH | `DELETE | `HEAD | `OPTIONS];
                 headers : (string * string) list;
                 body : string;
                 version : int * int; (* client HTTP version *)
                 id : int; (* request id *)
                 socket : Unix.file_descr;
                 line : string; (** request line *)
                 mutable blocking : unit IO.output option; (* hack for forked childs *)
                 encoding : encoding;
                 }

type reply_status =
  [ `Ok
  | `Created
  | `Found
  | `Moved
  | `Bad_request
  | `Unauthorized
  | `Forbidden
  | `Not_found
  | `Not_acceptable
  | `Conflict
  | `Length_required
  | `Request_too_large
  | `Internal_server_error
  | `Not_implemented
  | `Service_unavailable
  | `Version_not_supported
  | `Custom of string ]

type extended_reply_status = [ reply_status | `No_reply ]

type 'status reply' = 'status * (string * string) list * string
type reply = extended_reply_status reply'

let show_method = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `PATCH -> "PATCH"
  | `DELETE -> "DELETE"
  | `HEAD -> "HEAD"
  | `OPTIONS -> "OPTIONS"

let method_of_string = function
  | "GET" -> `GET
  | "POST" -> `POST
  | "PUT" -> `PUT
  | "PATCH" -> `PATCH
  | "DELETE" -> `DELETE
  | "HEAD" -> `HEAD
  | "OPTIONS" -> `OPTIONS
  | s -> Exn.fail "method_of_string %s" s

let show_client_addr req =
  let orig = Nix.show_addr req.addr in
  match req.addr with
  | Unix.ADDR_INET (addr,_) when addr = Unix.inet_addr_loopback -> (try List.assoc "x-real-ip" req.headers with Not_found -> orig)
  | _ -> orig

let client_addr req = match req.addr with Unix.ADDR_INET (addr,port) -> addr, port | _ -> assert false
let client_ip req = fst @@ client_addr req

let show_request req =
  sprintf "#%d %s time %.4f (recv %.4f) %s %s%s"
    req.id
    (show_client_addr req)
    (Time.get () -. req.conn)
    (req.recv -. req.conn)
    (show_method req.meth)
    (Exn.default "" (List.assoc "host") req.headers)
    req.url

let status_code : reply_status -> int = function
  | `Ok -> 200
  | `Created -> 201

  | `Moved -> 301
  | `Found -> 302

  | `Bad_request -> 400
  | `Unauthorized -> 401
  | `Forbidden -> 403
  | `Not_found -> 404
  | `Not_acceptable -> 406
  | `Conflict -> 409
  | `Length_required -> 411
  | `Request_too_large -> 413

  | `Internal_server_error -> 500
  | `Not_implemented -> 501
  | `Service_unavailable -> 503
  | `Version_not_supported -> 505

  | `Custom _ -> 999

let show_http_reply : reply_status -> string = function
  | `Ok -> "HTTP/1.0 200 OK"
  | `Created -> "HTTP/1.0 201 Created"

  | `Moved -> "HTTP/1.0 301 Moved Permanently"
  | `Found -> "HTTP/1.0 302 Found"

  | `Bad_request -> "HTTP/1.0 400 Bad Request"
  | `Unauthorized -> "HTTP/1.0 401 Unauthorized"
  | `Forbidden -> "HTTP/1.0 403 Forbidden"
  | `Not_found -> "HTTP/1.0 404 Not Found"
  | `Not_acceptable -> "HTTP/1.0 406 Not Acceptable"
  | `Conflict -> "HTTP/1.0 409 Conflict"
  | `Length_required -> "HTTP/1.0 411 Length Required"
  | `Request_too_large -> "HTTP/1.0 413 Request Entity Too Large"

  | `Internal_server_error -> "HTTP/1.0 500 Internal Server Error"
  | `Not_implemented -> "HTTP/1.0 501 Not Implemented"
  | `Service_unavailable -> "HTTP/1.0 503 Service Unavailable"
  | `Version_not_supported -> "HTTP/1.0 505 HTTP Version Not Supported"

  | `Custom s -> s

(* basically allow all *)
let auto_options req =
  let requested_headers =
    try
      List.assoc "access-control-request-headers" req.headers
    with _ -> ""
  in
  (`Ok, [
    "Allow", "OPTIONS, GET, POST, PUT, PATCH, DELETE, HEAD";
    "Access-Control-Allow-Origin", "*";
    "Access-Control-Allow-Headers", requested_headers; (* mimic *)
    "Access-Control-Max-Age", "600" (* try to cache *)
  ], "")
