include ExtThreadBase

let log = Log.self

type 'a t = [ `Exn of exn | `None | `Ok of 'a ] ref * Thread.t
let detach f x =
  let result = ref `None in
  result, Thread.create (fun () -> result := Exn.map f x) ()
let join (result,thread) = Thread.join thread; match !result with `None -> assert false | (`Ok _ | `Exn _ as x) -> x
let join_exn t = match join t with `Ok x -> x | `Exn exn -> raise exn
let map f a = Array.map join_exn @@ Array.map (detach f) a
let mapn ?(n=8) f l =
  assert (n > 0);
  Action.distribute n l |> map (List.map @@ Exn.map f) |> Action.undistribute

let locked mutex f = Mutex.lock mutex; Std.finally (fun () -> Mutex.unlock mutex) f ()

module LockMutex = struct
  type t = Mutex.t
  let create = Mutex.create
  let locked = locked
end

module Async_fin = struct

  open Async
  module U = ExtUnix.All

  type t = { q : (unit -> unit) Mtq.t; evfd : Unix.file_descr; }

  let is_available () = ExtUnix.Config.have `EVENTFD

  let setup events =
    let fin = { q = Mtq.create (); evfd = U.eventfd 0; } in
    let rec loop () =
      match Mtq.try_get fin.q with
      | None -> ()
      | Some f -> begin try f () with exn -> log #warn ~exn "fin loop" end; loop ()
    in
    let reset fd =
      try
        ignore (U.eventfd_read fd)
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _) -> ()
      | exn -> log #warn ~exn "fin reset"; ()
    in
    setup_simple_event events fin.evfd [Ev.READ] begin fun _ fd _ -> reset fd; loop () end;
    fin

  let shutdown { q; evfd } = Mtq.clear q; Unix.close evfd

  let callback fin f =
    Mtq.put fin.q f;
    U.eventfd_write fin.evfd 1L

end

let log_create ?name f x = Thread.create (fun () -> Action.log ?name f x) ()

let run_periodic ~delay ?(now=false) f =
  let (_:Thread.t) = Thread.create begin fun () ->
    if not now then Nix.sleep delay;
    while try f () with exn -> Log.self #warn ~exn "ExtThread.run_periodic"; true do
      Nix.sleep delay
    done
  end ()
  in
  ()

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

module Workers(T:WorkerT) =
struct

type task = T.task
type result = T.result
type t = task Mtq.t * result Mtq.t * int

let worker qi f qo =
  while true do
    Mtq.put qo (f (Mtq.get qi))
  done

let stop ?wait:_ (qi,_,_) = Mtq.clear qi

let create f n =
  let qi = Mtq.create () and qo = Mtq.create () in
  for _ = 1 to n do
    ignore (Thread.create (fun () -> worker qi f qo) ())
  done;
  qi,qo,n

let perform (qi,qo,n) ?autoexit:_ e f =
  let active = ref 0 in
  for _ = 1 to n do
    match Enum.get e with
    | Some x -> Mtq.put qi x; incr active
    | None -> ()
  done;
  while !active > 0 do
    let res = Mtq.get qo in
    begin match Enum.get e with
    | Some x -> Mtq.put qi x
    | None -> decr active
    end;
    f res
  done

end

let atomic_incr = incr
let atomic_decr = decr
let atomic_get x = !x

module Pool = struct

  type t = { q : (unit -> unit) Mtq.t;
             total : int;
             free : int ref;
             mutable blocked : bool;
             }

  let create n =
    let t = { q = Mtq.create (); total = n; free = ref (-1); blocked = false;} in t

  let init t =
    let worker _i =
      while true do
        let f = Mtq.get t.q in
        atomic_decr t.free;
        begin try f () with exn -> log #warn ~exn "ThreadPool" end;
        atomic_incr t.free;
      done
    in
    t.free := t.total;
    for i = 1 to t.total do
      let (_:Thread.t) = log_create worker i in ()
    done

  let status t = Printf.sprintf "queue %d threads %d of %d"
                    (Mtq.length t.q) (atomic_get t.free) t.total

  let put t =
    if atomic_get t.free = -1 then init t;
    while t.blocked do
      Nix.sleep 0.05
    done;
    Mtq.put t.q

  let wait_blocked ?(n=0) t =
    if (atomic_get t.free <> -1) then begin
      while t.blocked do Nix.sleep 0.05 done;(* Wait for unblock *)
      t.blocked <- true;
      assert(n>=0);
      let i = ref 1 in
      while Mtq.length t.q + (t.total - atomic_get t.free)> n do (* Notice that some workers can be launched! *)
        if !i = 100 || !i mod 1000 = 0 then
          log #info "Thread Pool - waiting block : %s" (status t);
        Nix.sleep 0.05;
        incr i
      done;
      t.blocked <- false
    end

end

(* Writes copy the path from the shard root to the leaf, then CAS the root. *)
module ShardedHashTrie = struct
  type 'k hashable = { equal : 'k -> 'k -> bool; hash : 'k -> int }

  (* [compare], not [(=)], as in Hashtbl: [nan] must be equal to itself *)
  let poly_hashable = { equal = (fun a b -> compare a b = 0); hash = Hashtbl.hash }

  let level_bits = 4
  let level_width = 1 lsl level_bits
  let level_mask = level_width - 1

  (* assoc list (all keys have same hash) *)
  type ('k, 'v) bindings =
    | Nil
    | Cons of { key : 'k; value : 'v; rest : ('k, 'v) bindings }

  type ('k, 'v) tree =
    | Empty
    | Leaf of { h : int; key : 'k; value : 'v; next : ('k, 'v) bindings }
      (** [h]: hash bits remaining at this depth; [next]: other keys with the same hash *)
    | Node of ('k, 'v) tree array (** [level_width] children *)

  type ('k, 'v) t = {
    hashable : 'k hashable;
    shard_bits : int;
    shard_mask : int;
    shards : ('k, 'v) tree Atomic.t array;
  }

  let create ?(hashable = poly_hashable) ?(shard_bits = 6) () =
    if shard_bits < 0 || shard_bits > 16 then invalid_arg "ShardedHashTrie.create: shard_bits must be in [0,16]";
    { hashable; shard_bits; shard_mask = 1 lsl shard_bits - 1;
      shards = Array.init (1 lsl shard_bits) (fun _ -> Atomic.make Empty) }

  let rec assoc equal k = function
    | Nil -> raise_notrace Not_found
    | Cons { key; value; rest } -> if equal k key then value else assoc equal k rest

  let rec find_tree equal h k = function
    | Empty -> raise_notrace Not_found
    | Leaf { h = h2; key; value; next } ->
      if h <> h2 then raise_notrace Not_found
      else if equal k key then value
      else assoc equal k next
    | Node a -> find_tree equal (h lsr level_bits) k a.(h land level_mask)

  (* shard and remaining hash computed inline: a helper returning both would allocate a tuple *)
  let find_notrace t k =
    let h = t.hashable.hash k in
    find_tree t.hashable.equal (h lsr t.shard_bits) k (Atomic.get t.shards.(h land t.shard_mask))

  (* re-raise so that callers get a backtrace *)
  let find t k = try find_notrace t k with Not_found -> raise Not_found

  let find_opt t k = match find_notrace t k with v -> Some v | exception Not_found -> None
  let mem t k = match find_notrace t k with _ -> true | exception Not_found -> false

  (* Pure: returns a new tree. [k] must not be bound in [tree]. *)
  let rec insert h k v tree =
    match tree with
    | Empty -> Leaf { h; key = k; value = v; next = Nil }
    | Leaf { h = h2; key; value; next } when h = h2 ->
      Leaf { h; key = k; value = v; next = Cons { key; value; rest = next } }
    | Leaf { h = h2; key; value; next } ->
      (* different hashes: replace the leaf with a node holding it one level
         down, and insert there. Terminates since [h] and [h2] differ in some bit. *)
      let a = Array.make level_width Empty in
      a.(h2 land level_mask) <- Leaf { h = h2 lsr level_bits; key; value; next };
      let i = h land level_mask in
      a.(i) <- insert (h lsr level_bits) k v a.(i);
      Node a
    | Node a ->
      let i = h land level_mask in
      let a = Array.copy a in
      a.(i) <- insert (h lsr level_bits) k v a.(i);
      Node a

  let get_or_create t k ~f =
    let h = t.hashable.hash k in
    let shard = t.shards.(h land t.shard_mask) in
    let h = h lsr t.shard_bits in
    let equal = t.hashable.equal in
    let tree = Atomic.get shard in
    match find_tree equal h k tree with
    | v -> v
    | exception Not_found ->
      let v = f k in
      (* invariant: [k] is not bound in [tree] *)
      let rec add tree =
        if Atomic.compare_and_set shard tree (insert h k v tree) then v
        else
          let tree = Atomic.get shard in
          match find_tree equal h k tree with
          | v' -> v' (* another domain bound [k] first: drop [v] *)
          | exception Not_found -> add tree
      in
      add tree

  let rec fold_bindings f acc = function
    | Nil -> acc
    | Cons { key; value; rest } -> fold_bindings f (f key value acc) rest

  let rec fold_tree f acc = function
    | Empty -> acc
    | Leaf { key; value; next; _ } -> fold_bindings f (f key value acc) next
    | Node a -> Array.fold_left (fold_tree f) acc a

  let fold t f acc =
    let snapshot = Array.map Atomic.get t.shards in
    Array.fold_left (fold_tree f) acc snapshot

  let iter t f = fold t (fun k v () -> f k v) ()
  let to_list t = fold t (fun k v acc -> (k, v) :: acc) []
  let length t = fold t (fun _ _ n -> n + 1) 0
  let clear t = Array.iter (fun shard -> Atomic.set shard Empty) t.shards
end
