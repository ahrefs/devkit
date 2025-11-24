[@@@ocaml.warning "-37"]
(* values are constructed from C *)

open Printf
open ExtLib
let ( let* ) = Result.bind

(* sync urlparse_stubs.c *)
type scheme =
  | Http
  | Https
[@@deriving ord, eq, show]

type t = {
  scheme : scheme;
  port : int;
  path : string list;
  query : (string * string list) list;
  fragment : string;
  host : string;
}
[@@deriving ord, eq, show]

type path = string list

type query = (string * string list) list

let debug = show

exception Malformed of string

let scheme u = u.scheme
let host u = u.host
let port u = u.port
let path u = u.path
let query u = u.query
let fragment u = if u.fragment <> "" then Some u.fragment else None

let rec args_of_query query =
  match query with
  | [] -> Ok []
  | (k, [ v ]) :: query ->
    let* args = args_of_query query in
    Ok ((k, v) :: args)
  | (k, _ :: _ :: _) :: _ -> Error (sprintf "key %S has multiple values" k)
  | (k, []) :: _ -> Error (sprintf "key %S has no value" k)

let args url = url |> query |> args_of_query

let query_of_args args = List.map (fun (k, v) -> k, [ v ]) args

let default_port = function
  | Http -> 80
  | Https -> 443

let default_scheme = function
  | 443 -> Https
  | _ -> Http

let make ?scheme ~host ?port ?(path = []) ?(query = []) ?(fragment = "") () =
  let scheme, port =
    match scheme, port with
    | None, None -> Http, 80
    | None, Some port -> default_scheme port, port
    | Some scheme, None -> scheme, default_port scheme
    | Some scheme, Some port -> scheme, port
  in
  if port <= 0 || port > 65535 then invalid_arg @@ sprintf "Url.make: %s port %d" host port;
  { scheme; host; port; path; query; fragment }

let make_args ?scheme ~host ?port ?path ?args ?fragment () =
  make ?scheme ?port ?path ?query:(Option.map query_of_args args) ?fragment ~host ()

let without_path u = {u with path=[]}
let without_query u = {u with query=[]}
let without_fragment u = { u with fragment = "" }
let without_parameters u = { u with query = []; fragment = "" }

let string_of_scheme = function
  | Http -> "http"
  | Https -> "https"

let decode = Web.rawurldecode

let decode_plus = Web.urldecode

module Re = struct

  open Tyre

  let list1 r = conv (fun (elt, li) -> elt :: li) (fun li -> List.hd li, List.tl li) (r <&> list r)

  module Charsets = struct
    open Charset

    let _gen_delims = set ":/?#[]@"

    let sub_delims  = set "!$&'()*+,;="

    let _reserved = _gen_delims || sub_delims

    let unreserved = ascii && (alpha || digit || set "-._~")

    let host = unreserved || sub_delims || char ':'

    let path = unreserved || sub_delims || char ':' || char '@'
  end

  let percent_encoded = matched_string (str "%" <* (xdigit <&> xdigit))

  let charset_or_pct_enc ~allow_empty ~decode chrs =
    non_greedy @@
    map decode @@
    matched_string
      ( (if allow_empty then list else list1)
          (matched_string (charset chrs) <|> percent_encoded)
      )

  let scheme = const Http (str "http") <|> const Https (str "https")

  let host = charset_or_pct_enc ~allow_empty:false ~decode Charsets.host

  let port = pos_int

  let path = non_greedy @@ list (str "/" *> charset_or_pct_enc ~allow_empty:true ~decode Charsets.path) <* opt (str "/")

  let query =
    let open Tyre in
    non_greedy
      begin
       separated_list ~sep:(str "&")
          begin
            let+ k = charset_or_pct_enc ~allow_empty:true ~decode Charsets.path
            and+ _ = str "="
            and+ vs = separated_list ~sep:(str ",") (charset_or_pct_enc ~allow_empty:true ~decode:decode_plus Charsets.path) in
            k, vs
          end
      end

  let fragment = (charset_or_pct_enc ~allow_empty:true ~decode:decode_plus Charsets.path)

  let url =
    let+ scheme = opt (scheme <* str "://")
    and+ host
    and+ port = opt (str ":" *> port)
    and+ path
    and+ query = opt (str "?" *> query)
    and+ fragment = opt (str "#" *> fragment)
  in
  make ?scheme ~host ?port ~path ?query ?fragment ()
end

let map_tyre_error r =
  r
  |> Result.map_error (function
    | `NoMatch _ -> "Invalid query string"
    | `ConverterFailure exn -> raise exn)

let parse_re re =
  let re = Tyre.compile Tyre.(start *> re <* stop) in
  fun str ->
    if String.length str > 128_000
    then (Error "String is longer than 128 000")
    else Tyre.exec re str |> map_tyre_error

let to_exn parse txt =
  match parse txt with
  | Ok v -> v
  | Error msg -> raise (Malformed msg)

let parse_query = parse_re Re.query
let parse = parse_re Re.url

let parse_query_exn = to_exn parse_query
let parse_exn = to_exn parse

let rec push_concat ~push ~sep f li =
  match li with
  | [ elt ] -> f elt
  | [] -> ()
  | elt :: (_ :: _ as li) ->
    f elt;
    push sep;
    push_concat ~push ~sep f li

let encode_plus = Web.urlencode

let encode = Web.rawurlencode

let push_path ~push path =
  push "/";
  path
  |> List.iter begin fun segment ->
    push (encode segment);
    push "/"
  end

let push_query ~push query =
  query
  |> push_concat ~push ~sep:"&" begin fun (k, vs) ->
    push (encode k);
    push "=";
    vs |> push_concat ~push ~sep:"," (fun v -> push (encode_plus v))
  end
let push_full_path ~push u =
  push_path ~push u.path;
  if u.query <> [] then begin
    push "?";
    push_query ~push u.query
  end;
  if u.fragment <> "" then begin
    push "#";
    push (encode u.fragment)
  end

let push_url ~push u =
  let host = u.host in
  push @@ string_of_scheme u.scheme;
  push "://";
  push host;
  if default_port u.scheme <> u.port then (
    push ":";
    push (string_of_int u.port));
  push_full_path ~push u

let string_of_push f a =
  let b = Buffer.create 256 in
  let push = Buffer.add_string b in
  f ~push a;
  Buffer.contents b

let to_string = string_of_push push_url

let full_path = string_of_push push_full_path

let query_to_string = string_of_push push_query

let is_root u = u.path = [] && u.query = [] && u.fragment = ""

let root url = { url with path = []; query = []; fragment = "" }

let hash u = Hashtbl.hash @@ to_string u

let with_host url host = { url with host }

let with_scheme url scheme =
  match scheme, url.scheme, url.port with
  | Http, Http, 80 | Https, Https, 443 -> url
  | Http, _, _ -> { url with scheme = Http; port = 80 }
  | Https, _, _ -> { url with scheme = Https; port = 443 }

let with_query url query = { url with query }

let with_path url path = { url with path }

let with_fragment url fragment = { url with fragment }

module Op = struct
  let ( / ) u segment = { u with path = u.path @ [ segment ] }

  let ( /? ) u args =
    if u.query <> [] then failwith "Query should be empty when using (/?)" else { u with query = query_of_args args }
end

include Op
