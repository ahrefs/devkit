(** Extensions to Enum *)

include module type of Enum with type 'a t = 'a Enum.t

(** same as {!Enum.find}, but found element is peeked, not junked *)
val find_peek : ('a -> bool) -> 'a t -> 'a

(** @return enum that indefinitely runs over given (non-empty) list *)
val list_loop : 'a list -> 'a t

(** @return enum over [DynArray] slice (default: whole array) *)
val of_dynarray : ?start:int -> ?n:int -> 'a DynArray.t -> 'a Enum.t

val dyn_range : ?start:int -> ?n:int -> 'a DynArray.t -> 'a Enum.t [@@ocaml.deprecated "use of_dynarray"]

(** [take n e] @return enum consuming first [n] elements of [e] *)
val take : int -> 'a t -> 'a t

(** merge two enums of same type *)
val align : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t

val join : ?left:bool -> ?right:bool -> ?multi:bool -> ('a -> 'b -> int) -> 'a t -> 'b t -> ('a option * 'b option) t
val join_assoc : ?left:bool -> ?right:bool -> ?multi:bool -> ('a -> 'a -> int) -> ('a * 'b) t -> ('a * 'c) t -> ('a * 'b option * 'c option) t

(** merge two enums of different types *)
val merge : ('a -> 'b -> int) -> 'a t -> 'b t -> ('a option * 'b option) t

(** merge two enums over key-value pairs *)
val merge_assoc : ('a -> 'a -> int) -> ('a * 'b) t -> ('a * 'c) t -> ('a * 'b option * 'c option) t

(** [group equal fold zero e]
  accumulates elements of [e] with [fold], first element is [fold]ed with [zero],
  at each subsequent step [equal] is checked, and new accumulator is started once it returns [false]
*)
val group : ('acc -> 'a -> bool) -> ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc t

(** [group_assoc equal fold zero e]
  accumulates (with [fold]) values in [e] with matching key as determined by comparison function [equal],
  first value is [fold]ed with [zero], e.g.:

  [List.of_enum @@ Enum.group_assoc (=) (+) 0 @@ List.enum \["a",1; "a",2; "b",3; "b",4; "a", 1; "a", 10\] = \["a", 3; "b", 7; "a", 11; \] ]
*)
val group_assoc : ('a -> 'a -> bool) -> ('b -> 'c -> 'b) -> 'b -> ('a * 'c) t -> ('a * 'b) t

(** [uniq f e] replaces every consecuitive sequence of elements from [e] comparing equal
  by the given comparison function [f] with the first element from that sequence *)
val uniq : ('a -> 'a -> bool) -> 'a t -> 'a t

(** [count_unique f e] replaces every consecutive sequence of elements from [e] comparing equal
  by the given comparison function [f] with the first element from that sequence and the number of duplicates *)
val count_unique : ('a -> 'a -> bool) -> 'a t -> ('a * int) t

(** [sub e f] extracts a subenum (consecutive sequence of the elements from [e]) that map to the same value of [f] *)
val sub : ?cmp:('b -> 'b -> bool) -> 'a t -> ('a -> 'b) -> ('b * 'a t) option

(** [iter_while f e] calls [f] for each element of [e] until it returns [false] or [e] is exhausted *)
val iter_while : ('a -> bool) -> 'a t -> unit
