open Devkit
module H = ExtThread.ShardedHashTrie

(* hash functions to exercise the trie: collisions, deep splits, sign and high bits *)
let hashables = [
  "poly", H.poly_hashable;
  "low byte", { H.equal = Int.equal; hash = (fun k -> k land 0xff) };
  "constant", { H.equal = Int.equal; hash = (fun _ -> 0) };
  "negative", { H.equal = Int.equal; hash = (fun k -> - (Hashtbl.hash k) - 1) };
  "high bits", { H.equal = Int.equal; hash = (fun k -> Hashtbl.hash k lsl 32) };
]

type op = Get_or_create of int * int | Find of int | Mem of int | Clear

let show_op = function
  | Get_or_create (k, v) -> Printf.sprintf "get_or_create %d %d" k v
  | Find k -> Printf.sprintf "find %d" k
  | Mem k -> Printf.sprintf "mem %d" k
  | Clear -> "clear"

let gen_ops =
  let open QCheck2.Gen in
  let* range = oneof_list [ 8; 1_000; 1_000_000 ] in
  let key = int_bound range in
  list_size (int_bound 2_000) @@ oneof_weighted [
    10, map2 (fun k v -> Get_or_create (k, v)) key nat;
    10, map (fun k -> Find k) key;
    3, map (fun k -> Mem k) key;
    1, pure Clear;
  ]

let sorted l = List.sort compare l

(* single domain: behaves like Hashtbl with add-if-absent *)
let model_test (name, hashable) =
  QCheck2.Test.make ~count:300 ~name:("model vs Hashtbl, " ^ name)
    ~print:QCheck2.Print.(list show_op) gen_ops
    (fun ops ->
       let t = H.create ~hashable ~shard_bits:2 () in
       let m = Hashtbl.create 16 in
       let step op =
         match op with
         | Get_or_create (k, v) ->
           let expected = match Hashtbl.find_opt m k with Some v -> v | None -> Hashtbl.add m k v; v in
           H.get_or_create t k ~f:(fun _ -> v) = expected
         | Find k -> H.find_opt t k = Hashtbl.find_opt m k
         | Mem k -> H.mem t k = Hashtbl.mem m k
         | Clear -> H.clear t; Hashtbl.reset m; true
       in
       List.for_all step ops
       && H.length t = Hashtbl.length m
       && sorted (H.to_list t) = sorted (List.of_seq (Hashtbl.to_seq m)))

let shard_bits_test =
  QCheck2.Test.make ~count:50 ~name:"any shard_bits"
    QCheck2.Gen.(pair (int_bound 16) (list_size (int_bound 500) nat))
    (fun (shard_bits, keys) ->
       let t = H.create ~shard_bits () in
       List.iter (fun k -> ignore (H.get_or_create t k ~f:Fun.id : int)) keys;
       List.for_all (fun k -> H.find t k = k) keys
       && H.length t = List.length (List.sort_uniq compare keys))

let invalid_shard_bits_test =
  QCheck2.Test.make ~count:1 ~name:"shard_bits out of range" QCheck2.Gen.unit (fun () ->
    List.for_all (fun shard_bits ->
      match H.create ~shard_bits () with _ -> false | exception Invalid_argument _ -> true)
      [ -1; 17 ])

(* Hashtbl.hash only looks at the first 10 meaningful values: these all collide *)
let structural_collision_test =
  QCheck2.Test.make ~count:50 ~name:"full hash collisions (long lists)"
    QCheck2.Gen.(list_size (int_range 1 200) nat)
    (fun tails ->
       let prefix = List.init 10 Fun.id in
       let keys = List.sort_uniq compare tails |> List.map (fun x -> prefix @ [ x ]) in
       let h0 = Hashtbl.hash (List.hd keys) in
       assert (List.for_all (fun k -> Hashtbl.hash k = h0) keys);
       let t = H.create () in
       List.iteri (fun i k -> ignore (H.get_or_create t k ~f:(fun _ -> i) : int)) keys;
       List.for_all2 (fun i k -> H.find t k = i) (List.init (List.length keys) Fun.id) keys
       && H.length t = List.length keys)

let long_strings_test =
  QCheck2.Test.make ~count:50 ~name:"long string keys"
    QCheck2.Gen.(list_size (int_bound 300) (string_size (int_range 100 2_000)))
    (fun keys ->
       let t = H.create ~hashable:{ H.equal = String.equal; hash = Hashtbl.hash } () in
       List.iter (fun k -> ignore (H.get_or_create t k ~f:String.length : int)) keys;
       List.for_all (fun k -> H.find t k = String.length k) keys
       && H.length t = List.length (List.sort_uniq compare keys))

let large_test (name, hashable) =
  let n = if name = "constant" then 2_000 else 200_000 in
  QCheck2.Test.make ~count:1 ~name:(Printf.sprintf "%d keys, %s" n name) QCheck2.Gen.unit (fun () ->
    let t = H.create ~hashable () in
    for k = 0 to n - 1 do ignore (H.get_or_create t k ~f:(fun k -> 2 * k) : int) done;
    let ok = ref (H.length t = n) in
    for k = 0 to n - 1 do if H.find t k <> 2 * k then ok := false done;
    !ok && not (H.mem t n) && H.fold t (fun _ _ c -> c + 1) 0 = n)

let domains = 4

(* all domains race on the same keys: each key must end up with a single value,
   and every caller must get that same (physical) value *)
let concurrent_get_or_create_test (name, hashable) =
  QCheck2.Test.make ~count:20 ~name:("concurrent get_or_create, " ^ name)
    QCheck2.Gen.(int_range 1 (if name = "constant" then 300 else 5_000))
    (fun n ->
       let t = H.create ~hashable ~shard_bits:1 () in
       let results = Array.init domains (fun d ->
         Domain.spawn (fun () ->
           (* different orders per domain to create contention everywhere *)
           Array.init n (fun i ->
             let k = if d land 1 = 0 then i else n - 1 - i in
             k, H.get_or_create t k ~f:(fun k -> ref k))))
         |> Array.map Domain.join
       in
       let per_key = Array.make n [] in
       Array.iter (Array.iter (fun (k, v) -> per_key.(k) <- v :: per_key.(k))) results;
       H.length t = n
       && Array.for_all (fun vs -> let v = H.find t !(List.hd vs) in List.for_all (fun v' -> v' == v) vs) per_key)

(* a key found once is found forever, while another domain keeps inserting *)
let concurrent_readers_test =
  QCheck2.Test.make ~count:10 ~name:"readers during writes" QCheck2.Gen.(int_range 1_000 50_000)
    (fun n ->
       let t = H.create ~shard_bits:2 () in
       let writer = Domain.spawn (fun () -> for k = 0 to n - 1 do ignore (H.get_or_create t k ~f:Fun.id : int) done) in
       let readers = List.init (domains - 1) (fun _ -> Domain.spawn (fun () ->
         let seen = ref 0 and ok = ref true in
         while !seen < n do
           (* everything below [seen] was found before, must still be there *)
           for k = max 0 (!seen - 100) to !seen - 1 do if H.find_opt t k <> Some k then ok := false done;
           if H.mem t !seen then incr seen else Domain.cpu_relax ()
         done;
         !ok)) in
       Domain.join writer;
       List.for_all Domain.join readers && H.length t = n)

let () =
  (* a broken trie tends to loop forever rather than fail: the default SIGALRM action kills us *)
  ignore (Unix.alarm 300 : int);
  let tests =
    List.map model_test hashables
    @ [ shard_bits_test; invalid_shard_bits_test; structural_collision_test; long_strings_test ]
    @ List.map large_test hashables
    @ List.map concurrent_get_or_create_test hashables
    @ [ concurrent_readers_test ]
  in
  exit (QCheck_base_runner.run_tests ~verbose:true tests)
