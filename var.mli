(** Global register for various types of counters *)

type t = Time of Time.t | Count of int | Bytes of int

(** [new typ type key] registers new [type] of counters with designated [key] label *)
class typ : string -> string ->
object
  method ref : 'a. 'a -> ('a -> t) -> string -> 'a ref
  method count : string -> int ref
  method bytes : string -> int ref
  method time : string -> float ref
end

(** [cc pp type key] new set of counters with designated [type] and [key] label *)
val cc : ('a -> string) -> string -> string -> 'a Cache.Count.t

(** [cc pp type key] new set of counters with designated [type] and [key] label, treated as milliseconds *)
val cc_ms : ('a -> string) -> string -> string -> 'a Cache.Count.t

(* val show : unit -> string *)
(** callback takes type, key label, key name and value *)
val iter : (string -> string -> string -> t -> unit) -> unit
