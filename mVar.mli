(** Variable shared between threads *)

type 'a t

(** Create *)
val create : unit -> 'a t

(** Set the variable (overwriting previous value if any) and return immediately *)
val set : 'a t -> 'a -> unit

(** Unset the variable *)
val clear : 'a t -> unit

(** Get value (block until it is available) *)
val get : 'a t -> 'a

(** Get value (block until it is available) and unset *)
val grab : 'a t -> 'a

(** Get value immediately without blocking
    @return None if value was not set *)
val try_get : 'a t -> 'a option

(** Grab value immediately without blocking
    @return None if value was not set *)
val try_grab : 'a t -> 'a option

