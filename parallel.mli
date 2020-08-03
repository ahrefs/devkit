(** Parallel *)

(** Invoke function in a forked process and return result *)
val invoke : ('a -> 'b) -> 'a -> unit -> 'b

(** Launch function for each element of the list in the forked process.
  Does not wait for children to finish - returns immediately. *)
val launch_forks : ('a -> unit) -> 'a list -> unit

(** Launch forks for each element of the list and wait for all workers to finish.
  Pass exit signals to the workers, see {!Forks.stop} for the description of [wait_stop] parameter.
  @param revive to keep workers running (restarting with same param if exited) [default: false]
*)
val run_forks : ?wait_stop:int -> ?revive:bool -> ?wait:int -> ?workers:int -> ('a -> unit) -> 'a list -> unit

(** Same as [run_forks] but do not fork for one worker *)
val run_forks' : ('a -> unit) -> 'a list -> unit

(** Process list with specified number of workers.
  Pass exit signals to the workers, see {!Forks.stop} for the description of [wait_stop] parameter.
*)
val run_workers : int -> ?wait_stop:int -> ('a -> unit) -> 'a list -> unit

(** Process enum with specified number of workers, collect results via provided callback.
  Pass exit signals to the workers, see {!Forks.stop} for the description of [wait_stop] parameter.
*)
val run_workers_enum : int -> ?wait_stop:int -> ('a -> 'b) -> ('b -> unit) -> 'a Enum.t -> unit

module type WorkerT = sig
  type task
  type result
end

module type Workers = sig

type task
type result
type t

(** [create f n] starts [n] parallel workers waiting for tasks *)
val create : (task -> result) -> int -> t

(** [perform workers tasks f] distributes [tasks] to all [workers] in parallel,
    collecting results with [f] and returns when all [tasks] are finished *)
val perform : t -> ?autoexit:bool -> task Enum.t -> (result -> unit) -> unit

(** [stop ?wait workers] kills worker processes with SIGTERM
  is [wait] is specified it will wait for at most [wait] seconds before killing with SIGKILL,
  otherwise it will wait indefinitely
  @param autoexit determines whether workers will exit once there are no more tasks, it means [perform] shouldn't be called again
    for this instance
*)
val stop : ?wait:int -> t -> unit

end (* Workers *)

(*
val create : ('a -> 'b) -> int -> ('a,'b) t
val perform : ('a,'b) t -> 'a Enum.t -> ('b -> unit) -> unit
*)

(** Forked workers *)
module Forks(T:WorkerT) : Workers
  with type task = T.task
   and type result = T.result

module Services : sig
  type t

  val start : int -> (int -> unit Lwt.t) -> t Lwt.t

  val rolling_restart : ?wait:int -> timeout:float -> t -> unit Lwt.t

  val stop : timeout:float -> t -> unit Lwt.t
end
