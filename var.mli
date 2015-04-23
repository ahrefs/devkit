(** Global register for various types of counters *)

type attributes = (string * string) list
type t = Time of Time.t | Count of int | Bytes of int

(** [new typ type ?attr key] registers new [type] of counters with designated [attr]ibutes and [key] name *)
class typ : string -> ?attr:attributes -> string ->
object
  method ref : 'a. 'a -> ('a -> t) -> string -> 'a ref
  method count : string -> int ref
  method bytes : string -> int ref
  method time : string -> float ref
end

(** [cc pp type ?attr key] new set of counters with designated [type], [attr]ibutes and [key] name *)
val cc : ('a -> string) -> string -> ?attr:attributes -> string -> 'a Cache.Count.t

(** [cc pp type ?attr key] new set of counters with designated [type], [attr]ibutes and [key] name, treated as milliseconds *)
val cc_ms : ('a -> string) -> string -> ?attr:attributes -> string -> 'a Cache.Count.t

(* val show : unit -> string *)
(** callback takes type, attributes, key name, key value and counter value *)
val iter : (string -> attributes -> string -> string -> t -> unit) -> unit
