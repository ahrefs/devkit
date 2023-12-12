open Netsys_types

type _ tstring_kind =
  | String_kind : string tstring_kind
  | Bytes_kind : Bytes.t tstring_kind

type 't tstring_ops = {
  kind : 't tstring_kind option;
  length : 't -> int;
  get : 't -> int -> char;
  unsafe_get : 't -> int -> char;
  unsafe_get3 : 't -> int -> int; (* get 3 chars packed into one int *)
  copy : 't -> 't;
  string : 't -> string;
  bytes : 't -> Bytes.t;
  sub : 't -> int -> int -> 't;
  substring : 't -> int -> int -> string;
  subbytes : 't -> int -> int -> Bytes.t;
  subpoly : 'u. 'u tstring_kind -> 't -> int -> int -> 'u;
  blit_to_bytes : 't -> int -> Bytes.t -> int -> int -> unit;
  index_from : 't -> int -> char -> int;
  index_from3 : 't -> int -> int -> char -> char -> char -> int;
  rindex_from : 't -> int -> char -> int;
  rindex_from3 : 't -> int -> int -> char -> char -> char -> int;
}

type tstring_ops_box =
  | Tstring_ops_box : 't tstring_kind * 't tstring_ops -> tstring_ops_box

type tstring_box =
  | Tstring_box : 't tstring_kind * 't tstring_ops * 't -> tstring_box

type tstring_polybox =
  | Tstring_polybox : 't tstring_ops * 't -> tstring_polybox
(* Warning: you cannot match on the type 't here *)

let str_subpoly : type u. u tstring_kind -> string -> int -> int -> u = function
  | String_kind -> String.sub
  | Bytes_kind ->
      fun s pos len ->
        let b = Bytes.create len in
        Bytes.blit_string s pos b 0 len;
        b

let str_index_from3 s p n c1 c2 c3 =
  (* FIXME: implement in C *)
  let sn = String.length s in
  if n < 0 || p < 0 || p > sn - n then invalid_arg "index_from3";
  let lim = p + n in
  let p = ref p in
  while
    !p < lim
    &&
    let c = String.unsafe_get s !p in
    c <> c1 && c <> c2 && c <> c3
  do
    incr p
  done;
  if !p >= lim then raise Not_found;
  !p

let str_rindex_from3 s p n c1 c2 c3 =
  (* FIXME: implement in C *)
  let sn = String.length s in
  if n < 0 || p < -1 || p >= sn || n - 1 > p then invalid_arg "rindex_from";
  let lim = p - n + 1 in
  let p = ref p in
  while
    !p >= lim
    &&
    let c = String.unsafe_get s !p in
    c <> c1 && c <> c2 && c <> c3
  do
    decr p
  done;
  if !p < lim then raise Not_found;
  !p

let string_ops =
  {
    kind = Some String_kind;
    length = String.length;
    get = String.get;
    unsafe_get = String.unsafe_get;
    unsafe_get3 =
      (fun s k ->
        let c0 = Char.code (String.unsafe_get s k) in
        let c1 = Char.code (String.unsafe_get s (k + 1)) in
        let c2 = Char.code (String.unsafe_get s (k + 2)) in
        (c0 lsl 16) lor (c1 lsl 8) lor c2);
    copy = (fun s -> s);
    (* ... for the time being ... *)
    string = (fun s -> s);
    bytes = Bytes.of_string;
    sub = String.sub;
    substring = String.sub;
    subbytes =
      (fun s p l ->
        let b = Bytes.create l in
        Bytes.blit_string s p b 0 l;
        b);
    subpoly = str_subpoly;
    blit_to_bytes = Bytes.blit_string;
    index_from = String.index_from;
    index_from3 = str_index_from3;
    rindex_from = String.rindex_from;
    rindex_from3 = str_rindex_from3;
  }

let bytes_index_from3 s p n c1 c2 c3 =
  str_index_from3 (Bytes.unsafe_to_string s) p n c1 c2 c3

let bytes_rindex_from3 s p n c1 c2 c3 =
  str_rindex_from3 (Bytes.unsafe_to_string s) p n c1 c2 c3

let bytes_subpoly : type u. u tstring_kind -> Bytes.t -> int -> int -> u =
  function
  | String_kind -> Bytes.sub_string
  | Bytes_kind -> Bytes.sub

let bytes_ops =
  {
    kind = Some Bytes_kind;
    length = Bytes.length;
    get = Bytes.get;
    unsafe_get = Bytes.unsafe_get;
    unsafe_get3 =
      (fun s k ->
        let c0 = Char.code (Bytes.unsafe_get s k) in
        let c1 = Char.code (Bytes.unsafe_get s (k + 1)) in
        let c2 = Char.code (Bytes.unsafe_get s (k + 2)) in
        (c0 lsl 16) lor (c1 lsl 8) lor c2);
    copy = Bytes.copy;
    string = Bytes.to_string;
    bytes = (fun s -> s);
    sub = Bytes.sub;
    substring = Bytes.sub_string;
    subbytes = Bytes.sub;
    subpoly = bytes_subpoly;
    blit_to_bytes = Bytes.blit;
    index_from = Bytes.index_from;
    index_from3 = bytes_index_from3;
    rindex_from = Bytes.rindex_from;
    rindex_from3 = bytes_rindex_from3;
  }

let ops_of_tstring = function
  | `String _ -> Tstring_ops_box (String_kind, string_ops)
  | `Bytes _ -> Tstring_ops_box (Bytes_kind, bytes_ops)

type 'a with_fun = { with_fun : 's. 's tstring_ops -> 's -> 'a }

let with_tstring : 'a with_fun -> tstring -> 'a =
 fun f -> function
  | `String s -> f.with_fun string_ops s
  | `Bytes s -> f.with_fun bytes_ops s

let length_tstring ts =
  with_tstring { with_fun = (fun ops s -> ops.length s) } ts

let polymorph_string_transformation :
    type s t. (string -> string) -> s tstring_ops -> t tstring_kind -> s -> t =
 fun f ops out_kind s ->
  let s' = f (ops.string s) in
  match out_kind with
  | String_kind -> s'
  | Bytes_kind -> Bytes.of_string s'

let tstring_of_tbuffer = function
  | `Bytes s -> `Bytes s
  | `String s -> `Bytes s
