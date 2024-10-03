(** HTML scanner *)

module Raw = HtmlStream_ragel.Raw

type tag = {
  name: string;
  attrs: (string * Raw.t) list;
  self_closing: bool;
}

type token_tag =
(* FIXME: also meh at Start <-> Close, should be open/close, start/end *)
| Start of tag
| Close of tag
| Text of Raw.t
| Script of ((string * Raw.t) list * string)
| Style of ((string * Raw.t) list * string)

(* legacy, kept for compatibility *)
type elem =
| Tag of (string * (string * Raw.t) list)
| Script of ((string * Raw.t) list * string) (** attributes and contents. TODO investigate script contents encoding *)
| Style of ((string * Raw.t) list * string)
| Text of Raw.t
| Close of string

type ctx

val init : unit -> ctx
val get_lnum : ctx -> int

(**
  Scan string for html tags.
  NB
  1. self-closing tags (e.g. [<x/>]) will result in two tags generated [<x></x>] (except for [<a/>])
  2. unfinished tags at the end of input are ignored
*)
val parse : ?ctx:ctx -> (elem -> unit) -> string -> unit
(* FIXME: meh at _new naming *)
val parse_new : ?ctx:ctx -> (token_tag -> unit) -> string -> unit

(** @return html string for [elem] *)
val show_raw : elem -> string

(** @return html string for [token_tag] *)
val show_raw_new : token_tag -> string

(** @return html string for [elem] using single quote for attributes *)
val show_raw' : elem -> string

val attrs_include : (string * Raw.t) list -> (string * string) list -> bool
val tag : string -> ?a:(string * string) list -> elem -> bool
val close : string -> elem -> bool

(** extract text from the list elements *)
val make_text : ?br:bool -> elem list -> Raw.t
