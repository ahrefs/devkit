(** Thread utilities *)

val locked : Mutex.t -> (unit -> 'a) -> 'a

type 'a t
val detach : ('a -> 'b) -> 'a -> 'b t
val join : 'a t -> 'a Exn.result
val join_exn : 'a t -> 'a

(** parallel Array.map *)
val map : ('a -> 'b) -> 'a array -> 'b array

(** parallel map with the specified number of workers, default=8 *)
val mapn : ?n:int -> ('a -> 'b) -> 'a list -> 'b Exn.result list

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

  (** @return if OS has necessary support for this module *)
  val is_available : unit -> bool

  val setup : Libevent.event_base -> t

  (** Destructor. All queued events are lost *)
  val shutdown : t -> unit

  (** Arrange for callback to be executed in libevent loop, callback should not throw (exceptions are reported and ignored) *)
  val callback : t -> (unit -> unit) -> unit

end

(** Create new thread wrapped in {!Action.log} *)
val log_create : ?name:string -> ('a -> unit) -> 'a -> Thread.t

(** run [f] in thread periodically once in [delay] seconds.
  @param f returns [false] to stop the thread, [true] otherwise
  @param now default [false]
*)
val run_periodic : delay:float -> ?now:bool -> (unit -> bool) -> unit

module type WorkerT = sig
  type task
  type result
end

module type Workers = sig
type task
type result
type t
val create : (task -> result) -> int -> t
val perform : t -> ?autoexit:bool -> task Enum.t -> (result -> unit) -> unit
val stop : ?wait:int -> t -> unit
end

(** Thread workers *)
module Workers(T:WorkerT) : Workers
  with type task = T.task
   and type result = T.result

module Pool : sig
type t
val create : int -> t
val status : t -> string
val put : t -> (unit -> unit) -> unit
val wait_blocked : ?n:int -> t -> unit
end
