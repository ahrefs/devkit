(** Useful shortcuts *)

module U = ExtUnix.Specific
module Enum = ExtEnum

val ( $ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
(** function composition : [f $ g] is equivalent to [(fun x -> f (g x))] *)
val ( $$ ) : ('a -> 'a -> 'b) -> ('c -> 'a) -> 'c -> 'c -> 'b
(** 2-function composition : [f $$ g] is equivalent to [(fun x y -> f (g x) (g y))] *)
val ( >> ) : 'a -> ('a -> 'b) -> 'b [@@ocaml.deprecated "use (|>) instead"]
(** @deprecated Use [|>] instead. *)
val ( & ) : ('a -> 'b) -> 'a -> 'b [@@ocaml.deprecated "use (@@) instead"]
(** @deprecated Use [\@\@] instead. *)

val id : 'a -> 'a
(** identity *)
val identity : 'a -> 'a
(** idem *)

val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
(** reverse arguments, [flip f x y] is equivalent to [f y x] *)

val apply2 : ('a -> 'b) -> 'a * 'a -> 'b * 'b
(** map over 2-tuple *)

val some : 'a -> 'a option
(** [some x] is equivalent to [Some x] *)
val const : 'a -> (unit -> 'a)
(** @return function returning given value *)
val curry : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
(** @return curried version from function of tuple *)
val uncurry : ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
(** @return function of tuple from curried function *)

val ( !! ) : 'a Lazy.t -> 'a
(** [Lazy.force] *)

val printfn : ('a, unit, string, unit) format4 -> 'a
(** printf to stdout with newline *)
val eprintfn : ('a, unit, string, unit) format4 -> 'a
(** printf to stderr with newline *)

(** abstract type generator *)
module New(T : sig type t end) :
sig
  type t
  val inj : T.t -> t
  val proj : t -> T.t
  val inj_list : T.t list -> t list
  val proj_list : t list -> T.t list
end

val tuck : 'a list ref -> 'a -> unit
val cons : 'a list -> 'a -> 'a list

val ( += ) : int ref -> int -> unit
val ( -= ) : int ref -> int -> unit

val round : float -> float

val atoi : string -> string -> int
(** [atoi name value]
  @return integer of string [value]
  @raise Failure if [value] is not an integer (with [name] and [value] in exception message)
*)

val call_me_maybe : ('a -> unit) option -> 'a -> unit
