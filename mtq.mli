(** Queue shared between multiple threads *)

type 'a t

(** Create queue *)
val create : unit -> 'a t

(** Put item into the queue and return immediately *)
val put : 'a t -> 'a -> unit

(** Get item from the queue (will block while queue is empty) *)
val get : 'a t -> 'a

(** Peek the item (leaving it in the queue) *)
val peek : 'a t -> 'a

(** Drop item from the queue if present *)
val junk : 'a t -> unit

(** Get item from the queue without blocking
    @return None immediately if queue is empty *)
val try_get : 'a t -> 'a option

(** Get the length of the queue *)
val length : 'a t -> int

(** Remove all elements from the queue *)
val clear : 'a t -> unit

