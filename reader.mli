(** Simple string reader *)

type t

exception EOS
exception Not_equal of string

val init : string -> t
val eos : t -> bool

(** post-condition: [eos] is true *)
val rest : t -> string
val till : t -> string -> string
val try_till : t -> string -> string
val tillc : t -> char -> string
val try_tillc : t -> char -> string
val take : t -> int -> string
val try_take : t -> int -> string
val is_const : t -> string -> bool
val const : t -> string -> unit
val try_const : t -> string -> unit
val while_ : t -> (char -> bool) -> string
val skipc : t -> char -> unit
