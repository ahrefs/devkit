(** Thread utilities *)

val check_main_domain : string -> unit
(** [check_main_domain name] raises [Failure] if not called from the main domain.
    Use it to guard process-wide setup and configuration. [name] identifies the caller in the error. *)

(** Domain-safe hash table, fast for reads.

    All operations may be called concurrently from any domain.
    Lookups are wait-free and do not allocate. Writes are lock-free: they copy
    a short path of the underlying trie and retry on contention, so inserts are
    about twice as slow as with [Hashtbl]. Meant for read-mostly tables, eg. a
    set of keys that is looked up far more often than it grows.

    There is no resizing: the table is split into a fixed number of shards
    (see [shard_bits] in {!create}), each holding a hash trie of branching
    factor 16 that deepens as it grows. *)
module ShardedHashTrie : sig
  type 'k hashable = { equal : 'k -> 'k -> bool; hash : 'k -> int }
  (** [equal a b] implies [hash a = hash b]. Keys with equal hashes are kept in a
      list, so a hash with few distinct values makes the table slow. *)

  val poly_hashable : 'k hashable
  (** Same as [Hashtbl]: [compare a b = 0] and [Hashtbl.hash]. For string keys, prefer
      [{ equal = String.equal; hash = Hashtbl.hash }]. *)

  type ('k, 'v) t

  val create : ?hashable:'k hashable -> ?shard_bits:int -> unit -> ('k, 'v) t
  (** @param hashable defaults to {!poly_hashable}
      @param shard_bits log2 of the number of shards, in [\[0,16\]]. Default 6
        (64 shards, about 1.5KB), fine for up to a few thousand keys; use 8 or more for larger tables.
      @raise Invalid_argument if [shard_bits] is out of range *)

  val find : ('k, 'v) t -> 'k -> 'v
  (** @raise Not_found if the key is not bound *)

  val find_opt : ('k, 'v) t -> 'k -> 'v option
  val mem : ('k, 'v) t -> 'k -> bool

  val get_or_create : ('k, 'v) t -> 'k -> f:('k -> 'v) -> 'v
  (** Return the value bound to the key, binding it to [f key] first if absent.
      Concurrent callers for the same absent key may each call [f], but only one
      result is stored and all of them get that one. *)

  (** {2 Iteration}

      Iteration works on a snapshot of the shards taken at the start, so writes
      made during iteration are not observed. The snapshot is not atomic across
      shards. Order is unspecified. *)

  val iter : ('k, 'v) t -> ('k -> 'v -> unit) -> unit
  val fold : ('k, 'v) t -> ('k -> 'v -> 'acc -> 'acc) -> 'acc -> 'acc
  val to_list : ('k, 'v) t -> ('k * 'v) list

  val length : ('k, 'v) t -> int
  (** Linear time: counts the bindings of a snapshot, like {!fold} *)

  val clear : ('k, 'v) t -> unit
  (** Not atomic across shards *)
end

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
