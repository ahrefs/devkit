(** In-memory cache *)

open Prelude
open Control

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
    locked t.mutex (fun () -> t.m <- M.add (key, x) (M.filter (not & on_key key) t.m))

  let get t key =
    let found = locked t.mutex (fun () -> M.filter (on_key key) t.m) in
    try M.choose found >> snd >> some with Not_found -> None

end


