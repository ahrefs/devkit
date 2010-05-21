(** In-memory cache *)

open ExtLib

open Prelude
open Control

(** Thread safe *)
module TimeLimited(E: sig type t end) = struct

  type key = Int64.t

  let fixed f = 10000. *. f >> Int64.of_float
  let current () = Unix.gettimeofday () >> fixed

  module Value = struct
    type t = key * E.t
    let compare (t1,_) (t2,_) = compare t1 t2
  end

  module M = Set.Make(Value)

  type t = { limit : Int64.t; mutex : Mutex.t; mutable m : M.t; }

  let private_refresh m = 
    let cur = current () in 
    if (try fst (M.min_elt m) <= cur with _ -> false)
    then M.filter (fun (l,_) -> l > cur) m
    else m

  let create limit = { limit = fixed limit; mutex = Mutex.create (); m = M.empty }
  let add t x = 
    let key = Int64.add t.limit (current ()) in 
    locked t.mutex (fun () -> t.m <- M.add (key, x) (private_refresh t.m)); 
    key

  let on_key key = fun (k,_) -> k = key

  let replace t key x =
    locked t.mutex (fun () -> t.m <- M.add (key, x) (M.filter (not $ on_key key) t.m))

  let get t key =
    let found = locked t.mutex (fun () -> M.filter (on_key key) t.m) in
    try M.choose found >> snd >> some with Not_found -> None

  let count t = locked t.mutex (fun () -> M.cardinal t.m)

end

module NoLock = struct
  type t = unit
  let create () = ()
  let locked () f = f ()
end

module LockMutex = struct
  type t = Mutex.t
  let create = Mutex.create
  let locked = Control.locked
end

module TimeLimited2(E: Set.OrderedType)
  (Lock: sig type t val create : unit -> t val locked : t -> (unit -> 'a) -> 'a end) = struct

  type time = Int64.t

  let fixed f = 10000. *. f >> Int64.of_float
  let current () = Unix.gettimeofday () >> fixed

  module Value = struct
    type t = E.t * time
    let compare (v1,_) (v2,_) = E.compare v1 v2
  end

  module M = Set.Make(Value)

  type t = { limit : time; mutable next : time; lock : Lock.t; mutable m : M.t; }

  let private_purge t =
    let cur = current () in
    if cur >= t.next then
    begin
      t.next <- Int64.add t.limit cur;
      t.m <- M.filter (fun (_,t) -> t > cur) t.m
    end

  let create limit = { limit = fixed limit; next = 0L; lock = Lock.create (); m = M.empty }
 
  let add t x =
    let expire = Int64.add t.limit (current ()) in
    (* FIXME replace not add *)
    Lock.locked t.lock (fun () -> private_purge t; t.m <- M.add (x, expire) t.m)

  let get t v =
    (* lock is not needed *)
    let found = Lock.locked t.lock (fun () -> M.filter (fun (x,_) -> 0 = E.compare x v) t.m) in
    try M.choose found >> some with Not_found -> None

  let count t = Lock.locked t.lock (fun () -> M.cardinal t.m)

  let iter t f = Lock.locked t.lock (fun () -> M.iter (fun (x,_) -> f x) t.m)

end

(** Count elements *)
module Count =
struct
  open Hashtbl
  let create () = create 100
  let add t x = match find_option t x with None -> add t x 1 | Some n -> replace t x (n+1)
  let enum t = enum t
  let show t f = enum t >> 
    Enum.map (fun (k,v) -> Printf.sprintf "%s: %u" (f k) v) >>
    Stre.concat " "
end

module Group : sig
  type ('a,'b) t
  val by : ('a -> 'b) -> ('a,'b) t
  val add : ('a,'b) t -> 'a -> unit
  val get : ('a,'b) t -> 'b -> 'a list
end = struct
  type ('a,'b) t = ('b,'a) Hashtbl.t * ('a -> 'b)
  let by f = Hashtbl.create 32, f
  let add (h,f) x = Hashtbl.add h (f x) x
  let get (h,_) k = Hashtbl.find_all h k
end

(** One-to-one associations *)
module Assoc : sig
  type ('a,'b) t
  val create : unit -> ('a,'b) t
  (** Add association, assert on duplicate key *)
  val add : ('a,'b) t -> 'a -> 'b -> unit
  (** Get associated value, @raise Not_found if key is not present *)
  val get : ('a,'b) t -> 'a -> 'b
  (** Get associated value *)
  val try_get : ('a,'b) t -> 'a -> 'b option
  (** Delete association, assert if key is not present, @return associated value *)
  val del : ('a,'b) t -> 'a -> 'b
  (** Delete association, assert if key is not present *)
  val remove : ('a,'b) t -> 'a -> unit
  val size : ('a,'b) t -> int
end = struct
  type ('a,'b) t = ('a,'b) Hashtbl.t
  let create () = Hashtbl.create 32
  let add h k v =
    assert (false = Hashtbl.mem h k);
    Hashtbl.add h k v
  let get = Hashtbl.find
  let try_get = Hashtbl.find_option
  let del h k =
    try 
      let v = Hashtbl.find h k in
      Hashtbl.remove h k; v
    with
      Not_found -> assert false
  let remove h k =
    assert (true = Hashtbl.mem h k);
    Hashtbl.remove h k
  let size = Hashtbl.length
end

