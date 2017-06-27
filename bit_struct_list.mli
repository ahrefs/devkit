(** Packed representation for list of integers of fixed bit length *)

module type S = sig
  (** number of bits to represent each item *)
  val item_bits : int
  val pp : int -> string
end

module Make(S: S) : sig
  type t
  val of_list : int list -> t
  (** [of_list l] converts a list of int values [l], into a bit structure list. *)

  val to_list : t -> int list
  (** [to_list b] converts [b] into a list of int values. *)

  val project : t -> string
  (** [project b] returns an internal string representation of [b]. *)

  val inject : string -> t
  (** [inject s] initializes a bit list with an internal string representation [s], previously returned by [project]. *)

  val iter : (int -> unit) -> t -> unit
  (** [iter f b] applies function [f] in turn to each item in [b]. *)

  val iterwhile : (int -> bool) -> t -> bool
  (** [iterwhile f b] applies function [f] in turn to each item in [b] until [f] returns [false]. *)

  val fold_left : ('a -> int -> 'a) -> 'a -> t -> 'a
  (** [fold_left f a b] applies function [f] in turn to each item in [b]
     and passes the result of previous step, similarly to {!List.fold_left}. *)

  val exists : (int -> bool) -> t -> bool
  (** [exists p b] checks if at least one element of [b] satisfies the predicate [p]. *)

  val pp : t -> string
  (** [pp b] returns a pretty-print string using [S.pp] for each item of [b]. *)
end
