open Prelude
open Control

open ExtLib
open Printf

(** Thread safe *)
module TimeLimited(E: sig type t end) = struct

  type key = Int64.t

  let fixed f = 10000. *. f |> Int64.of_float
  let current () = Unix.gettimeofday () |> fixed

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
    try M.choose found |> snd |> some with Not_found -> None

  let count t = locked t.mutex (fun () -> M.cardinal t.m)

end

module type Lock = sig
  type t
  val create : unit -> t
  val locked : t -> (unit -> 'a) -> 'a
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

  let fixed f = 10000. *. f |> Int64.of_float
  let current () = Unix.gettimeofday () |> fixed

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
    try M.choose found |> some with Not_found -> None

  let count t = Lock.locked t.lock (fun () -> M.cardinal t.m)

  let iter t f = Lock.locked t.lock (fun () -> M.iter (fun (x,_) -> f x) t.m)

end

module SizeLimited = struct

  type key = int

  let key = id

  type 'a t = { mutable arr : 'a array option; size : int; mutable key : key; mutex : Mutex.t; }

  let create size = { arr = None; size = size; key = 0; mutex = Mutex.create (); }

  let add t x = locked t.mutex (fun () ->
    let arr = match t.arr with | Some a -> a | None -> let a = Array.make t.size x in t.arr <- Some a; a in
    arr.(t.key mod t.size) <- x;
    t.key <- t.key + 1;
    t.key - 1)

  let get t k = locked t.mutex (fun () ->
    match t.arr with
    | None -> None
    | Some a -> if k < t.key && t.key <= k + t.size then Some (a.(k mod t.size)) else None)

  let random t = locked t.mutex (fun () ->
    match t.arr,t.key with
    | None,_ | _,0 -> None
    | Some a, key -> Some a.(Random.int key mod t.size))

end

module Count = struct
  open Hashtbl
  type 'a t = ('a,int) Hashtbl.t
  let create () : 'a t = create 16
  let clear = Hashtbl.clear
  let plus t x n = try replace t x (find t x + n) with Not_found -> Hashtbl.add t x n
  let minus t x n = try replace t x (find t x - n) with Not_found -> Hashtbl.add t x (0 - n)
  let of_enum e = let h = create () in Enum.iter (fun (k,n) -> plus h k n) e; h
  let of_list l = of_enum @@ List.enum l
  let add t x = plus t x 1
  let del t x = minus t x 1
  let enum t = enum t
  let iter t k = iter k t
  let fold t f acc = Hashtbl.fold f t acc
  let count t k = Option.default 0 @@ Hashtbl.find_option t k
  let count_all t = Hashtbl.fold (fun _ n acc -> acc + n) t 0
  let size = Hashtbl.length
  let show t ?(sep=" ") f = enum t |>
    List.of_enum |> List.sort ~cmp:(Action.compare_by fst) |>
    List.map (fun (x,n) -> sprintf "%S: %u" (f x) n) |>
    String.concat sep
  let show_sorted t ?limit ?(sep="\n") f = enum t |>
    List.of_enum |> List.sort ~cmp:(flip @@ Action.compare_by snd) |>
    (match limit with None -> id | Some n -> List.take n) |>
    List.map (fun (x,n) -> sprintf "%6d : %S" n (f x)) |>
    String.concat sep
  let stats t ?(cmp=compare) f =
    if Hashtbl.length t = 0 then
      "<empty>"
    else
      let a = Array.of_enum (enum t) in
      let total = Array.fold_left (fun t (_,n) -> t + n) 0 a in
      let half = total / 2 in
      let cmp (x,_) (y,_) = cmp x y in
      Array.sort cmp a;
      let med = ref None in
      let (mi,ma,_) = Array.fold_left begin fun (mi,ma,sum) x ->
        let sum = sum + snd x in
        if !med = None && half <= sum then med := Some x;
        let mi = if snd x < snd mi then x else mi in
        let ma = if snd x > snd ma then x else ma in
        mi, ma, sum
      end ((fst a.(0), max_int), (fst a.(0),min_int), 0) a
      in
      let show (x,n) = sprintf "%S (%d)" (f x) n in
      sprintf "total %d median %s min %s max %s"
        total (match !med with None -> "?" | Some x -> show x) (show mi) (show ma)
  let distrib t =
    if Hashtbl.length t = 0 then
      [||]
    else
      let a = Array.of_enum (enum t) in
      let total = Array.fold_left (fun t (_,n) -> t + n) 0 a in
      let limits = Array.init 10 (fun i -> total * (i + 1) / 10) in
      let cmp (x,_) (y,_) = compare (x:float) y in
      Array.sort cmp a;
      let distrib = limits |> Array.map begin fun limit ->
        let (v,_) = Array.fold_left begin fun (found,sum) (v,n) ->
          let sum = sum + n in
          if found = None && limit <= sum then Some v, sum else (found,sum)
        end (None,0) a in
        match v with
        | None -> nan
        | Some v -> v
      end
      in
      distrib
  let show_distrib ?(sep="\n") t =
    distrib t |> Array.mapi (fun i v -> sprintf "%d%% <= %f" ((i + 1) * 10) v) |> Array.to_list |> String.concat sep
  let report t ?limit ?cmp ?(sep="\n") f =
    let data = show_sorted t ?limit ~sep f in
    let stats = stats t ?cmp f in
    stats^sep^data
  let names (t : 'a t) = List.of_enum @@ Hashtbl.keys t
end

module Group = struct
  type ('a,'b) t = ('b,'a list) Hashtbl.t * ('a -> 'b)
  let by f = Hashtbl.create 32, f
  let add (h,f) x = let k = f x in try Hashtbl.replace h k (x :: Hashtbl.find h k) with Not_found -> Hashtbl.add h k [x]
  let get (h,_) k = try Hashtbl.find h k with Not_found -> []
  let iter (h,_) k = Hashtbl.iter k h
  let keys (h,_) = Hashtbl.keys h
end

let group_fst e =
  let h = Hashtbl.create 10 in
  Enum.iter (fun (k,v) -> Hashtbl.replace h k (try v :: Hashtbl.find h k with Not_found -> [v])) e;
  Hashtbl.enum h

module Assoc = struct
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

module Lists = struct
type ('a,'b) t = ('a,'b list) Hashtbl.t
let create () = Hashtbl.create 16
let get h k = try Hashtbl.find h k with Not_found -> []
let set = Hashtbl.replace
let add h k v = Hashtbl.replace h k (v::get h k)
let enum = Hashtbl.enum
let clear = Hashtbl.clear
let count_keys = Hashtbl.length
let count_all h = Hashtbl.fold (fun _ l acc -> acc + List.length l) h 0
end

class ['a] cache (cb : ('a list -> unit)) ~limit =
object(self)
  val mutable l = []
  method name = "cache"
  method add x =
    l <- x :: l;
    if List.length l >= limit then
    begin
      cb l;
      self#clear
    end
  method get = l
  method dump = cb l; l <- []
  method clear = l <- []
  method to_list = l
  method size = List.length l
end

type 'a reused = { cache : 'a Stack.t; create : (unit -> 'a); reset : ('a -> unit); }
let reuse create reset = { cache = Stack.create (); create; reset; }
let use t = if Stack.is_empty t.cache then t.create () else Stack.pop t.cache
let recycle t x = t.reset x; Stack.push x t.cache

module Reuse(T : sig type t val create : unit -> t val reset : t -> unit end) : sig
type t = T.t
val get : unit -> t
val release : t -> unit
end = struct
type t = T.t
let cache = reuse T.create T.reset
let get () = use cache
let release x = recycle cache x
end
