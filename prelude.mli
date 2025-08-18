(** Useful shortcuts *)

module U = ExtUnix.Specific
module Enum = ExtEnum

(** function composition : [f $ g] is equivalent to [(fun x -> f (g x))] *)
val ( $ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

(** 2-function composition : [f $$ g] is equivalent to [(fun x y -> f (g x) (g y))] *)
val ( $$ ) : ('a -> 'a -> 'b) -> ('c -> 'a) -> 'c -> 'c -> 'b

(** 1-argument function composition combinators that allow quickly switching to or from point-free chained function pipeline
  e.g. [let inc s = string_of_int @@ (+) 1 @@ int_of_string s] vs [let inc = F1.(string_of_int @@ (+) 1 @@ int_of_string)]
  and similarly [let inc = F1.(int_of_string |> (+) 1 |> string_of_int)]
*)
module F1 : sig
  val ( @@ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
  val ( |> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
end

(** identity *)
val id : 'a -> 'a

(** idem *)
val identity : 'a -> 'a

(** reverse arguments, [flip f x y] is equivalent to [f y x] *)
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

(** map over 2-tuple *)
val apply2 : ('a -> 'b) -> 'a * 'a -> 'b * 'b

(** [some x] is equivalent to [Some x] *)
val some : 'a -> 'a option

(** @return function returning given value *)
val const : 'a -> (unit -> 'a)

(** @return curried version from function of tuple *)
val curry : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)

(** @return function of tuple from curried function *)
val uncurry : ('a -> 'b -> 'c) -> ('a * 'b -> 'c)

(** [Lazy.force] *)
val ( !! ) : 'a Lazy.t -> 'a

(** printf to stdout with newline *)
val printfn : ('a, unit, string, unit) format4 -> 'a

(** printf to stderr with newline *)
val eprintfn : ('a, unit, string, unit) format4 -> 'a

(** abstract type generator *)
module Fresh(T : sig type t val compare : t -> t -> int end)() :
sig
  type t
  val inject : T.t -> t
  val project : t -> T.t
  val inject_list : T.t list -> t list
  val project_list : t list -> T.t list
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

val tuck : 'a list ref -> 'a -> unit
val cons : 'a list -> 'a -> 'a list

val ( += ) : int ref -> int -> unit
val ( -= ) : int ref -> int -> unit

val round : float -> float

(** [atoi name value]
  @return integer of string [value]
  @raise Failure if [value] is not an integer (with [name] and [value] in exception message)
*)
val atoi : string -> string -> int

val call_me_maybe : ('a -> unit) option -> 'a -> unit
