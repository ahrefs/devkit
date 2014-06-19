open Printf

type encoding = Gzip | Identity

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
                 encoding : encoding;
                 }

let show_method = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `HEAD -> "HEAD"

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
