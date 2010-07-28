(** Parallel *)

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
(** [kill ?wait workers] kills worker processes, waiting for at most [wait] seconds *)
val kill : ?wait:float -> t -> unit
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

