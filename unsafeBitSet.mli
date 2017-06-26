(*
  This is reduced copy of ExtLib.BitSet with removed safety checks and auto-resize code to get some more speed of it.
  It is about 15% faster than the original implementation.

  Update 2: converted to bigarray
 *)

type t

val create : int -> t
(** Create an empty bitset with an initial size (in number of bits). *)

val copy : t -> t
(** Copy a bitset : further modifications of first one will not affect the
 copy. *)

val set : t -> int -> unit
(** [set s n] sets the nth-bit in the bitset [s] to true. *)

val unset : t -> int -> unit
(** [unset s n] sets the nth-bit in the bitset [s] to false. *)

val put : t -> bool -> int -> unit
(** [put s v n] sets the nth-bit in the bitset [s] to [v]. *)

val toggle : t -> int -> unit
(** [toggle s n] changes the nth-bit value in the bitset [s]. *)

val is_set : t -> int -> bool
(** [is_set s n] returns true if nth-bit in the bitset [s] is set,
 or false otherwise. *)
