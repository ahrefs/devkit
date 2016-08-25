(** Simple wrapper over Lwt_condition, starts to wait again on condvar right after current [wait] was finished, to not lose signals.
    Usable when there is one thread that waits for "flag".
    "Multiple waiters" semantics is not defined here ( <-> Lwt_condition.broadcast), don't use it.
 *)

type 'a t
val create : unit -> 'a t
val signal : 'a t -> 'a -> unit
val wait : 'a t -> 'a Lwt.t
