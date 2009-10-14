(** Parallel *)

module type WorkerT = sig 
  type task 
  type result 
end

(** Forked workers *)
module Workers(T : WorkerT) : sig

type t

(** [create f n] starts [n] parallel workers waiting for tasks *)
val create : (T.task -> T.result) -> int -> t

(** [perform workers tasks f] distributes [tasks] to all [workers] in parallel,
    collecting results with [f] and returns when all [tasks] are finished *)
val perform : t -> T.task Enum.t -> (T.result -> unit) -> unit

end

