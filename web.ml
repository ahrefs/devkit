(** web utilities *)

open ExtLib
open Printf

open Prelude
open Control

let log = Log.self

(** percent-encode (convert space into %20) *)
let rawurlencode = Netencoding.Url.encode ~plus:false
(** percent-encode, but convert space into plus, not %20 *)
let urlencode = Netencoding.Url.encode ~plus:true
(** percent-decode (leave plus as is) *)
let rawurldecode s = try Netencoding.Url.decode ~plus:false s with _ -> s
(** percent-decode and convert plus into space *)
let urldecode s = try Netencoding.Url.decode ~plus:true s with _ -> s

let htmlencode = Netencoding.Html.encode ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 ()
let htmldecode = Netencoding.Html.decode ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 ()
let make_url_args = String.concat "&" $ List.map (fun (k, v) -> k ^ "=" ^ urlencode v)

(** Minimum strictness, Neturl will fail on malformed parameters in url *)
let parse_url_args args =
  String.nsplit args "&" |>
  List.filter_map (fun s -> try String.split s "=" |> apply2 urldecode |> some with _ -> None)

let url_get_args url = try String.split url "?" |> snd |> parse_url_args with _ -> []

let () = Curl.global_init Curl.CURLINIT_GLOBALALL

module CurlCache = Cache.Reuse(struct type t = Curl.t let create = Curl.init let reset = Curl.reset end)

let curl_default_setup h =
  Curl.set_nosignal h true;
  Curl.set_connecttimeout h 30;
  Curl.set_timeout h 60;
  Curl.set_followlocation h false;
  Curl.set_encoding h Curl.CURL_ENCODING_ANY;
  ()

type http_action_old =
[ `GET
| `POST_FORM of (string * string) list
| `POST of (string * string) (** content-type and body *)
| `PUT of (string * string)
| `DELETE
| `CUSTOM of (string * string * string) (** request, content-type and body *)
]

type http_body =
[ `Raw of string * string (** content-type and body *)
| `Form of (string * string) list (* key value *)
]

type http_action =
[ `GET
| `POST
| `PUT
| `DELETE
]

module type IO_TYPE = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val bracket : 'a t -> ('a -> unit t) -> ('a -> 'b t) -> 'b t
end

module type CURL = sig
  type 'a t
  val perform : Curl.t -> Curl.curlCode t
end

module Http (IO : IO_TYPE) (Curl_IO : CURL with type 'a t = 'a IO.t) = struct

  open IO

  let simple_result = function
    | `Ok (code, s) when code / 100 = 2 -> `Ok s
    | `Error code -> `Error (sprintf "(%d) %s" (Curl.errno code) (Curl.strerror code))
    | `Ok (n, _) -> `Error (sprintf "http %d" n)

  let return_unit = return ()

  let with_curl f = bracket (return @@ Curl.init ()) (fun h -> Curl.cleanup h; return_unit) f
  let with_curl_cache f = bracket (return @@ CurlCache.get ()) (fun h -> CurlCache.release h; return_unit) f

  let http_gets ?(setup=ignore) ?(check=(fun _ -> true)) ?(result=(fun _ _ -> return_unit)) url =
    with_curl_cache begin fun h ->
      Curl.set_url h url;
      curl_default_setup h;
      let () = setup h in
      let b = Buffer.create 10 in
      Curl.set_writefunction h begin fun s ->
        match check h with
        | true -> Buffer.add_string b s; String.length s
        | false -> 0
      end;
      Curl_IO.perform h >>= fun code ->
      result h code >>= fun () ->
      return @@ match code with
      | Curl.CURLE_OK -> `Ok (Curl.get_httpcode h, Buffer.contents b)
      | code -> `Error code
    end

  (* NOTE don't forget to set http_1_0=true when sending requests to a Httpev-based server *)
  let http_request' ?ua ?timeout ?(verbose=false) ?(setup=ignore) ?(http_1_0=false) ?body (action:http_action) url =
    let open Curl in
    let set_body h ct body =
      set_httpheader h ["Content-Type: "^ct];
      set_postfields h body;
      set_postfieldsize h (String.length body)
    in
    let setup h =
      begin match action with
      | `GET -> ()
      | `DELETE -> set_customrequest h "DELETE"
      | `POST -> set_post h true
      | `PUT -> set_post h true; set_customrequest h "PUT"
      end;
      begin match body with
      | Some (`Form args) -> set_body h "application/x-www-form-urlencoded" (make_url_args args)
      | Some (`Raw (ct,body)) -> set_body h ct body
      | None -> set_readfunction h (fun _ -> "") (* prevent reading from stdin with POST without body *)
      end;
      if http_1_0 then set_httpversion h HTTP_VERSION_1_0;
      Option.may (set_timeout h) timeout;
      Option.may (set_useragent h) ua;
      let () = setup h in
      ()
    in
    if verbose then begin
      let action = match action with `GET -> "GET" | `DELETE -> "DELETE" | `POST -> "POST" | `PUT -> "PUT" in
      let body = match body with
      | None -> ""
      | Some (`Form args) -> String.concat " " @@ List.map (fun (k,v) -> sprintf "%s=%S" k (Stre.shorten 64 v)) args
      | Some (`Raw (ct,body)) -> sprintf "%s %s" ct (Stre.shorten 64 body)
      in
      log #info "%s %s %s" action url body
    end;
    http_gets ~setup url

  let http_request ?ua ?timeout ?verbose ?setup ?http_1_0 ?body (action:http_action) url =
    http_request' ?ua ?timeout ?verbose ?setup ?http_1_0 ?body action url >>= fun res ->
    return @@ simple_result res

  let http_query ?ua ?timeout ?verbose ?setup ?http_1_0 ?body (action:http_action) url =
    let body = match body with Some (ct,s) -> Some (`Raw (ct,s)) | None -> None in
    http_request ?ua ?timeout ?verbose ?setup ?http_1_0 ?body action url

  let http_submit ?ua ?timeout ?verbose ?setup ?http_1_0 ?(action=`POST) url args =
    http_request ?ua ?timeout ?verbose ?setup ?http_1_0 ~body:(`Form args) action url

end

module IO_blocking = struct
  type 'a t = 'a
  let return = identity
  let ( >>= ) m f = f m
  let bracket = bracket
end

module Curl_blocking = struct
  type 'a t = 'a
  let perform h = try Curl.perform h; Curl.CURLE_OK with Curl.CurlException (code,_,_) -> code
end

module Http_blocking = Http(IO_blocking)(Curl_blocking)
let with_curl = Http_blocking.with_curl
let with_curl_cache = Http_blocking.with_curl_cache
let http_gets = Http_blocking.http_gets
let http_request' = Http_blocking.http_request'
let http_request = Http_blocking.http_request
let http_query = Http_blocking.http_query
let http_submit = Http_blocking.http_submit

module IO_lwt = struct
  type 'a t = 'a Lwt.t
  let return = Lwt.return
  let ( >>= ) = Lwt.( >>= )
  let bracket mresource destroy k =
    lwt resource = mresource in
    try_lwt
      k resource
    finally
      destroy resource
end

module Curl_lwt_for_http = struct
  type 'a t = 'a Lwt.t
  include Curl_lwt
end

module Http_lwt = Http(IO_lwt)(Curl_lwt_for_http)
let http_request_lwt' = Http_lwt.http_request'
let http_request_lwt = Http_lwt.http_request
let http_query_lwt = Http_lwt.http_query
let http_submit_lwt = Http_lwt.http_submit

let http_get_io_exn ?(setup=ignore) ?(check=(fun h -> Curl.get_httpcode h = 200)) url out =
  let inner = ref None in
  try
    with_curl_cache begin fun h ->
      Curl.set_url h url;
      curl_default_setup h;
      setup h;
      Curl.set_writefunction h (fun s ->
        try
          match check h with
          | false -> 0
          | true -> IO.nwrite out s; String.length s
        with exn -> inner := Some exn; 0);
      let result = Curl.perform h in
      IO.flush out;
      result
    end
  with
  | exn -> raise (Option.default exn !inner)

let http_get_io url ?(verbose=true) ?setup out =
  try
    http_get_io_exn url ?setup out
  with
  | Curl.CurlException(Curl.CURLE_WRITE_ERROR,_,_) -> ()
  | exn -> if verbose then Log.main #warn ~exn "http_get_io(%s)" url else ()

let http_get ?verbose ?setup url = wrapped (IO.output_string ()) IO.close_out (http_get_io ?verbose ?setup url)

let http_get_io_lwt ?body ?timeout ?(setup=ignore) ?(check=(fun h -> Curl.get_httpcode h = 200)) url out =
  let inner_error = ref `None in
  let error code = sprintf "curl (%d) %s" (Curl.errno code) (Curl.strerror code) in
  let inner_error_msg () =
    match !inner_error with
    | `None -> error Curl.CURLE_WRITE_ERROR
    | `Write exn -> sprintf "write error : %s" @@ Exn.to_string exn
    | `Http code -> sprintf "http : %d" code
  in
  try_lwt
    Http_lwt.with_curl_cache begin fun h ->
      Curl.set_url h url;
      curl_default_setup h;
      Option.may (Curl.set_timeout h) timeout;
      Option.may (fun (ct, body) ->
            let open Curl in
            set_post h true;
            set_httpheader h ["Content-Type: " ^ ct];
            set_postfields h body;
            set_postfieldsize h (String.length body)
        ) body;
      setup h;
      Curl.set_writefunction h begin fun s ->
        try
          match check h with
          | false -> inner_error := `Http (Curl.get_httpcode h); 0
          | true -> IO.nwrite out s; String.length s
        with exn -> inner_error := `Write exn; 0
      end;
      match_lwt Curl_lwt.perform h with
      | Curl.CURLE_OK when not @@ check h -> `Error (sprintf "http: %d"  (Curl.get_httpcode h)) |> Lwt.return
      | Curl.CURLE_OK -> IO.flush out; `Ok (Curl.get_sizedownload h) |> Lwt.return
      | Curl.CURLE_WRITE_ERROR -> `Error (inner_error_msg ()) |> Lwt.return
      | code -> `Error (error code) |> Lwt.return
    end
  with
  | exn -> Exn_lwt.fail ~exn "http_get_io_lwt (%s)" (inner_error_msg ())

(* NOTE don't forget to set http_1_0=true when sending requests to a Httpev-based server *)
let http_do ?ua ?timeout ?(verbose=false) ?(setup=ignore) ?(http_1_0=false) (action:http_action_old) url =
  let open Curl in
  let post ?req h ct body =
    set_post h true;
    begin match req with None -> () | Some s -> set_customrequest h s end;
    set_postfields h body;
    set_postfieldsize h (String.length body);
    set_httpheader h ["Content-Type: "^ct];
  in
  let setup h =
    begin match action with
    | `GET -> ()
    | `DELETE -> set_customrequest h "DELETE"
    | `POST (ct,body) -> post h ct body
    | `PUT (ct,body) -> post ~req:"PUT" h ct body
    | `POST_FORM args -> post h "application/x-www-form-urlencoded" (make_url_args args)
    | `CUSTOM (req,ct,body) -> post ~req h ct body
    end;
    if http_1_0 then set_httpversion h HTTP_VERSION_1_0;
    Option.may (set_timeout h) timeout;
    Option.may (set_useragent h) ua;
    let () = setup h in
    ()
  in
  if verbose then begin
    let log_verb req ct body = log #info "%s %s %s %s" req url ct (Stre.shorten 64 body) in
    match action with
    | `GET -> log #info "GET %s" url
    | `DELETE -> log #info "DELETE %s" url
    | `POST (ct,body) -> log_verb "POST" ct body
    | `PUT (ct,body) -> log_verb "PUT" ct body
    | `POST_FORM l -> log #info "POST %s %s" url (String.concat " " @@ List.map (fun (k,v) -> sprintf "%s=%S" k (Stre.shorten 64 v)) l)
    | `CUSTOM (req,ct,body) -> log_verb req ct body
  end;
  match http_gets ~setup url with
  | `Ok (code, s) when code / 100 = 2 -> `Ok s
  | `Error code -> `Error (sprintf "(%d) %s" (errno code) (strerror code))
  | `Ok (n, _) -> `Error (sprintf "http %d" n)

(* http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html *)
let string_of_http_code = function
| 100 -> "Continue"
| 101 -> "Switching protocols"

| 200 -> "OK"
| 201 -> "Created"
| 202 -> "Accepted"
| 203 -> "Non-Authoritative Information"
| 204 -> "No Content"
| 205 -> "Reset Content"
| 206 -> "Partial Content"

| 300 -> "Multiple Choices"
| 301 -> "Moved Permanently"
| 302 -> "Found"
| 303 -> "See Other"
| 304 -> "Not Modified"
| 305 -> "Use Proxy"
| 306 -> "(Unused)"
| 307 -> "Temporary Redirect"

| 400 -> "Bad Request"
| 401 -> "Unauthorized"
| 402 -> "Payment Required"
| 403 -> "Forbidden"
| 404 -> "Not Found"
| 405 -> "Method Not Allowed"
| 406 -> "Not Acceptable"
| 407 -> "Proxy Authentication Required"
| 408 -> "Request Timeout"
| 409 -> "Conflict"
| 410 -> "Gone"
| 411 -> "Length Required"
| 412 -> "Precondition Failed"
| 413 -> "Request Entity Too Large"
| 414 -> "Request-URI Too Long"
| 415 -> "Unsupported Media Type"
| 416 -> "Requested Range Not Satisfiable"
| 417 -> "Expectation Failed"

| 500 -> "Internal Server Error"
| 501 -> "Not Implemented"
| 502 -> "Bad Gateway"
| 503 -> "Service Unavailable"
| 504 -> "Gateway Timeout"
| 505 -> "HTTP Version Not Supported"

| _ -> "(Unknown)"

let class_of_http_code code =
  match code / 100 with
  | 1 -> "Informational"
  | 2 -> "Successful"
  | 3 -> "Redirection"
  | 4 -> "Client Error"
  | 5 -> "Server Error"
  | n -> sprintf "%dxx" n
