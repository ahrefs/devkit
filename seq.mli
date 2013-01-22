
exception Empty

type 'a t

val make : next:(unit -> 'a) -> peek:(unit -> 'a) -> 'a t
val from : (unit -> 'a) -> 'a t
val init : int -> (int -> 'a) -> 'a t
val empty : 'a t

val next : 'a t -> 'a option
val peek : 'a t -> 'a option
val next_exn : 'a t -> 'a
val peek_exn : 'a t -> 'a
val junk : 'a t -> unit
val is_empty : 'a t -> bool
val reset : 'a t -> unit

val iter : ('a -> unit) -> 'a t -> unit
val map : ('a -> 'b) -> 'a t -> 'b t
val filter_map : ('a -> 'b option) -> 'a t -> 'b t
