(** Global register for various types of counters *)

type attributes = (string * string) list
type t = Time of Time.t | Count of int | Bytes of int

(** [new typ type ?attr key] registers new [type] of counters with designated [attr]ibutes and [key] name *)
class typ : string -> ?attr:attributes -> string ->
object
  method ref : 'a. 'a -> ('a -> t) -> string -> 'a ref
  method get_count : string -> (unit -> int) -> unit
  method get_bytes : string -> (unit -> int) -> unit
  method get_time : string -> (unit -> Time.t) -> unit
  method count : string -> int ref
  method bytes : string -> int ref
  method time : string -> float ref
end

(** [cc pp type ?attr key] new set of counters with designated [type], [attr]ibutes and [key] name *)
val cc : ('a -> string) -> string -> ?attr:attributes -> string -> 'a Cache.Count.t

(** [cc pp type ?attr key] new set of counters with designated [type], [attr]ibutes and [key] name, treated as milliseconds *)
val cc_ms : ('a -> string) -> string -> ?attr:attributes -> string -> 'a Cache.Count.t

(* val show : unit -> string *)
(** callback takes attributes and value *)
val iter : (attributes -> t -> unit) -> unit
