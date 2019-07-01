(** HTML scanner *)

module Raw = HtmlStream_ragel.Raw

type elem =
| Tag of (string * (string * Raw.t) list)
| Script of ((string * Raw.t) list * string) (** attributes and contents. TODO investigate script contents encoding *)
| Style of ((string * Raw.t) list * string)
| Text of Raw.t
| Close of string

type ctx

val init : unit -> ctx
val get_lnum : ctx -> int

val parse : ?ctx:ctx -> (elem -> unit) -> string -> unit

(** @return html string for [elem] *)
val show_raw : elem -> string

(** @return html string for [elem] using single quote for attributes *)
val show_raw' : elem -> string

val attrs_include : (string * Raw.t) list -> (string * string) list -> bool
val tag : string -> ?a:(string * string) list -> elem -> bool
val close : string -> elem -> bool

(** extract text from the list elements *)
val make_text : ?br:bool -> elem list -> Raw.t
