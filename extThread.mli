(** Thread utilities *)

val locked : Mutex.t -> (unit -> 'a) -> 'a

module LockMutex : sig
  type t
  val create : unit -> t
  val locked : t -> (unit -> 'a) -> 'a
end

(**
  Communication from worker threads to the main event loop
*)
module Async_fin : sig

  type t

  val setup : Libevent.event_base -> t

  (** Arrange for callback to be executed in libevent loop, callback should not throw (exceptions are reported and ignored) *)
  val callback : t -> (unit -> unit) -> unit

end
