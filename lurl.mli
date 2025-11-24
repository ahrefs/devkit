type scheme =
  | Http
  | Https
[@@deriving ord, eq]

type t [@@deriving ord, eq]

type path = string list

type query = (string * string list) list

(** does not have :// *)
val string_of_scheme : scheme -> string

(** prints each part of the url like record fields. *)
val debug : t -> string

val make_args :
  ?scheme:scheme ->
  host:string ->
  ?port:int ->
  ?path:path ->
  ?args:(string * string) list ->
  ?fragment:string ->
  unit ->
  t

(** Similar to !{make_args}, except it takes a {!type-query} instead of [args].

GET queries can actually have multiple values associated with each key, this gives support for this.
*)
val make :
  ?scheme:scheme -> host:string -> ?port:int -> ?path:string list -> ?query:query -> ?fragment:string -> unit -> t

val parse : string -> (t, string) result

(** Malformed URL. *)
exception Malformed of string

(** Like {!parse}. Can raise {!Malformed} *)
val parse_exn : string -> t

(** parses a query string. Should not start with [?]. *)
val parse_query : string -> (query, string) result

(** can raise {!Malformed} *)
val parse_query_exn : string -> query

val query_to_string : query -> string

val to_string : t -> string

module Op : sig
  (** [u / segment] is the url [u] with [segment] appened to the end of its path. *)
  val ( / ) : t -> string -> t

  (** [u /? args] is the url [u] with the GET query arguments [args].
    It fails if [u] already has GET query arguments. *)
  val ( /? ) : t -> (string * string) list -> t
end

include module type of Op

val scheme : t -> scheme

val host : t -> string

val port : t -> int

val path : t -> string list

val query : t -> (string * string list) list

(** Fails if a key has a different number of values than 1. *)
val args : t -> ((string * string) list, string) result

(** [fragment u] is the fragment of url [u], that is the part after ["#"] that can point to an anchor in the page. *)
val fragment : t -> string option

val with_path : t -> path -> t

val with_host : t -> string -> t

val with_query : t -> query -> t

val with_fragment : t -> string -> t

val with_scheme : t -> scheme -> t

val hash : t -> int
val root : t -> t

val without_path: t -> t
val without_query: t -> t
val without_fragment : t -> t

(** [without_paramaters u] is [u] without its path, query and fragment. *)
val without_parameters : t -> t

val full_path : t -> string

val is_root : t -> bool
