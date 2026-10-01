(* Compare ExtThread.ShardedHashTrie (64 and 256 shards) with Saturn.Htbl, a flat bucketed assoc list,
   Hashtbl + Mutex, and (on one domain) a plain Hashtbl.

   dune exec --release bench/bench_sharded_hash_trie.exe *)

open Devkit

type ('k, 'v) ops = { find : 'k -> 'v; get_or_create : 'k -> f:('k -> 'v) -> 'v }
type 'k impl = { name : string; make : unit -> ('k, int Atomic.t) ops }

module Impls (K : Hashtbl.HashedType) = struct
  let devkit ?(shard_bits = 6) () = { name = Printf.sprintf "devkit/s%d" shard_bits; make = fun () ->
    let t = ExtThread.ShardedHashTrie.create ~shard_bits ~hashable:{ equal = K.equal; hash = K.hash } () in
    { find = ExtThread.ShardedHashTrie.find t; get_or_create = (fun k ~f -> ExtThread.ShardedHashTrie.get_or_create t k ~f) } }

  let saturn = { name = "saturn"; make = fun () ->
    let t = Saturn.Htbl.create ~hashed_type:(module K) () in
    let rec get_or_create k ~f =
      match Saturn.Htbl.find_exn t k with
      | v -> v
      | exception Not_found -> let v = f k in if Saturn.Htbl.try_add t k v then v else get_or_create k ~f
    in
    { find = Saturn.Htbl.find_exn t; get_or_create } }

  (* fixed buckets of assoc lists, the simplest lock-free option *)
  let flat = { name = "flat128"; make = fun () ->
    let b = Array.init 128 (fun _ -> Atomic.make []) in
    let bucket k = b.(K.hash k land 127) in
    let rec assoc k = function [] -> raise_notrace Not_found | (k', v) :: tl -> if K.equal k k' then v else assoc k tl in
    let find k = assoc k (Atomic.get (bucket k)) in
    let rec get_or_create k ~f =
      let b = bucket k in
      let l = Atomic.get b in
      match assoc k l with
      | v -> v
      | exception Not_found -> let v = f k in if Atomic.compare_and_set b l ((k, v) :: l) then v else get_or_create k ~f
    in
    { find; get_or_create } }

  let mutex = { name = "hashtbl+mutex"; make = fun () ->
    let module T = Hashtbl.Make (K) in
    let t = T.create 16 and m = Mutex.create () in
    { find = (fun k -> Mutex.protect m (fun () -> T.find t k));
      get_or_create = (fun k ~f -> Mutex.protect m (fun () ->
        match T.find t k with v -> v | exception Not_found -> let v = f k in T.add t k v; v)) } }

  (* not domain-safe: baseline for 1-domain runs only *)
  let plain = { name = "hashtbl (unsafe)"; make = fun () ->
    let module T = Hashtbl.Make (K) in
    let t = T.create 16 in
    { find = T.find t;
      get_or_create = (fun k ~f -> match T.find t k with v -> v | exception Not_found -> let v = f k in T.add t k v; v) } }

  let tries = [ devkit (); devkit ~shard_bits:8 () ]
  let all = tries @ [ saturn; flat; mutex ]
  let growing = tries @ [ saturn; mutex ] (* flat128 degrades to long lists *)
end

module S = Impls (struct include String let hash = Hashtbl.hash end)
module I = Impls (struct type t = int let equal = Int.equal let hash = Hashtbl.hash end)

let counter _ = Atomic.make 0

let on_domains n f = List.init n (fun d -> Domain.spawn (fun () -> f d)) |> List.iter Domain.join

let report ~title ~ops_per_call samples =
  Printf.printf "\n== %s\n" title;
  List.iter (fun (name, ts) ->
    let rates = List.map (fun (t : Benchmark.t) -> Int64.to_float t.iters *. float ops_per_call /. t.wall /. 1e6) ts in
    let rates = List.sort compare rates in
    Printf.printf "  %-16s %8.1f Mops/s (median of %d)\n%!" name (List.nth rates (List.length rates / 2)) (List.length rates))
    samples

(* the unsynchronized Hashtbl only makes sense on one domain *)
let with_plain domains impls plain = if domains = 1 then impls @ [ plain ] else impls

let bench ~title ~ops_per_call impls run =
  let samples = Benchmark.throughputN ~style:Benchmark.Nil ~repeat:5 1
      (List.map (fun impl -> impl.name, run, impl) impls) in
  report ~title ~ops_per_call samples

(* Cache.Count: a few constant keys, hit on every call *)
let count_like domains =
  let keys = Array.init 20 (Printf.sprintf "metric_name_%d") in
  let per_domain = 1_000_000 in
  bench ~title:(Printf.sprintf "count-like: 20 string keys, hits + incr, %d domain(s)" domains)
    ~ops_per_call:(per_domain * domains) (with_plain domains S.all S.plain)
    (fun impl ->
       let t = impl.make () in
       Array.iter (fun k -> ignore (t.get_or_create k ~f:counter)) keys;
       on_domains domains (fun d ->
         for i = 0 to per_domain - 1 do
           Atomic.incr (t.get_or_create keys.((i + d) mod 20) ~f:counter)
         done))

(* growing table: every call is a miss and an insert *)
let inserts domains =
  let n = 200_000 in
  bench ~title:(Printf.sprintf "inserts: %d fresh int keys, %d domain(s)" n domains)
    ~ops_per_call:n (with_plain domains I.all I.plain)
    (fun impl ->
       let t = impl.make () in
       on_domains domains (fun d ->
         let i = ref d in
         while !i < n do ignore (t.get_or_create !i ~f:counter); i := !i + domains done))

(* read-mostly: 10k warm keys, one insert of a new key every [insert_every] ops, finds otherwise.
   The table grows by [per_domain / insert_every] keys per domain and call. *)
let mixed ~label ~insert_every domains =
  let warm = 10_000 and per_domain = 2_000_000 in
  bench ~title:(Printf.sprintf "%s: %d warm int keys, 1 insert per %d ops, %d domain(s)" label warm insert_every domains)
    ~ops_per_call:(per_domain * domains) (with_plain domains I.growing I.plain)
    (fun impl ->
       let t = impl.make () in
       for k = 0 to warm - 1 do ignore (t.get_or_create k ~f:counter) done;
       on_domains domains (fun d ->
         let fresh = ref (warm + d) in
         for i = 0 to per_domain - 1 do
           if i mod insert_every = 0 then (ignore (t.get_or_create !fresh ~f:counter); fresh := !fresh + domains)
           else ignore (t.find ((i * 7919) mod warm))
         done))

let mixes = [ "mixed90", 10; "mixed99", 100; "mixed99.9", 1000 ]
let all_mixed () = List.iter (fun (label, insert_every) -> List.iter (mixed ~label ~insert_every) [ 1; 4; 8 ]) mixes

(* sanity check every implementation before timing it *)
let self_check () =
  List.iter (fun impl ->
    let t = impl.make () in
    let n = 100_000 in
    on_domains 4 (fun d -> let i = ref d in while !i < n do ignore (t.get_or_create !i ~f:(fun k -> Atomic.make k)); i := !i + 4 done);
    for k = 0 to n - 1 do if Atomic.get (t.find k) <> k then failwith (impl.name ^ ": self-check failed") done)
    I.all

let alloc_per_hit () =
  Printf.printf "\n== minor words per hit (20 string keys, 1 domain)\n";
  let keys = Array.init 20 (Printf.sprintf "metric_name_%d") in
  List.iter (fun impl ->
    let t = impl.make () in
    Array.iter (fun k -> ignore (t.get_or_create k ~f:counter)) keys;
    let n = 1_000_000 in
    let before = Gc.minor_words () in
    for i = 0 to n - 1 do ignore (t.get_or_create keys.(i mod 20) ~f:counter) done;
    Printf.printf "  %-16s %6.2f\n%!" impl.name ((Gc.minor_words () -. before) /. float n))
    S.all

let () =
  self_check ();
  match Sys.argv with
  | [| _; "mixed" |] -> all_mixed ()
  | [| _; "single" |] ->
    count_like 1;
    inserts 1;
    List.iter (fun (label, insert_every) -> mixed ~label ~insert_every 1) mixes
  | _ ->
    alloc_per_hit ();
    List.iter count_like [ 1; 4; 8 ];
    List.iter inserts [ 1; 4 ];
    all_mixed ()
