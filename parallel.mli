(** Parallel *)

(** Invoke function in a forked process and return result *)
val invoke : ('a -> 'b) -> 'a -> unit -> 'b

(** Launch function for each element of the list in the forked process.
  Does not wait for children to finish - returns immediately. *)
val launch_forks : ('a -> unit) -> 'a list -> unit

(** Launch forks for each element of the list and wait for all workers to finish. Pass exit signals to the workers. *)
val run_forks : ('a -> unit) -> 'a list -> unit

module Thread : sig
type 'a t
val detach : ('a -> 'b) -> 'a -> 'b t
val join : 'a t -> [ `Ok of 'a | `Exn of exn ]
val join_exn : 'a t -> 'a

(** parallel Array.map *)
val map : ('a -> 'b) -> 'a array -> 'b array
end

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
val perform : t -> task Enum.t -> (result -> unit) -> unit
(** [stop ?wait workers] kills worker processes with SIGTERM
  is [wait] is specified it will wait for at most [wait] seconds before killing with SIGKILL,
  otherwise it will wait indefinitely *)
val stop : ?wait:int -> t -> unit
end

(*
val create : ('a -> 'b) -> int -> ('a,'b) t
val perform : ('a,'b) t -> 'a Enum.t -> ('b -> unit) -> unit
*)

(** Thread workers *)
module Threads(T:WorkerT) : Workers
  with type task = T.task
   and type result = T.result 

(** Forked workers *)
module Forks(T:WorkerT) : Workers
  with type task = T.task
   and type result = T.result

module ThreadPool : sig
type t
val create : int -> t
val status : t -> string
val put : t -> (unit -> unit) -> unit
val wait_blocked : ?n:int -> t -> unit
end
