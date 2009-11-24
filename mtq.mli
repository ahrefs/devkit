(** Queue shared between multiple threads *)

type 'a t

(** Create queue *)
val create : unit -> 'a t

(** Put item into the queue and return immediately *)
val put : 'a t -> 'a -> unit

(** Get item from the queue (will block while queue is empty) *)
val get : 'a t -> 'a

(** Remove all elements from the queue *)
val clear : 'a t -> unit

