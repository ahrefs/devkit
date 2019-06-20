(** Global register for various types of counters *)

type attributes = (string * string) list
type t = Time of Time.t | Count of int | Bytes of int

val show_a : (string * string) list -> string
val is_in_families : string -> bool

(** [new typ type ?attr key] registers new [type] of counters with designated [attr]ibutes and [key] name *)
class typ : string -> ?attr:attributes -> string ->
object
  method ref : 'a. 'a -> ('a -> t) -> string -> 'a ref
  method get_count : string -> (unit -> int option) -> unit
  method get_bytes : string -> (unit -> int option) -> unit
  method get_time : string -> (unit -> Time.t option) -> unit
  method count : string -> int ref
  method bytes : string -> int ref
  method time : string -> float ref
  method unregister : unit -> unit
  method get : (string * t) list
  method show : string
end

(** [cc pp type ?attr key] new set of counters with designated [type], [attr]ibutes and [key] name

Logstash events will have attributes as follows :
 * all of [attr] key value pairs (if given)
 * class=[type]
 * [key]=X where X is value inserted into [CC]

Guidelines for picking names :
  keep number of different [key] names low (makes ES happy),
  uniqueness of events is primarily provided by [class].

Bad example :
  let pages = new Var.cc "tool.pages" "pages"
  let index = new Var.cc "tool.index" "index"
  let count = new Var.cc "tool.count" "count"

Better :
  let pages = new Var.cc "tool.pages" "kind"
  let pages = new Var.cc "tool.index" "kind"
  let pages = new Var.cc "tool.count" "kind"
*)
val cc : ('a -> string) -> string -> ?attr:attributes -> string -> 'a Cache.Count.t

(** [cc pp type ?attr key] new set of counters with designated [type], [attr]ibutes and [key] name, treated as milliseconds *)
val cc_ms : ('a -> string) -> string -> ?attr:attributes -> string -> 'a Cache.Count.t

(* val show : unit -> string *)
(** callback takes attributes and value *)
val iter : (attributes -> t -> unit) -> unit

(** [list_stats filter]

    @return a list containing a printed line for each counter whose type is in [filter].
*)
val list_stats : string list -> string list
