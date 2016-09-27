
open OUnit
open Printf
open ExtLib

open Prelude

let tests = ref []
let test name f = let open OUnit in tests := (name >:: f) :: !tests

let () = test "HtmlStream" begin fun () ->
  Printexc.record_backtrace true;
  let module HS = HtmlStream in
  let (==>) s s' =
  try
    let s'' = Control.wrapped_output (IO.output_string ()) (fun io -> Stream.iter (IO.nwrite io $ HS.show_raw') (HS.parse (Stream.of_string s))) in
    if s' = s'' then () else
      failwith (sprintf "%s ==> %s (got %s)" s s' s'')
  with
  | Failure s -> assert_failure s
  | exn -> assert_failure (sprintf "%s ==> %s (exn %s)\n%s" s s' (Exn.str exn) (Printexc.get_backtrace ()))
  in
  "<q>dsds<qq>" ==> "<q>dsds<qq>";
  "<>" ==> "<>";
  "< q>" ==> "<>";
  "<q>" ==> "<q>";
  "<q><b>dsad</b></Q><Br/><a a a>" ==> "<q><b>dsad</b></q><br><a a='' a=''>";
  "<q x= a=2><q x a=2><q a=2/><q AAaa=2 />" ==> "<q x='a'><q x='' a='2'><q a='2'><q aaaa='2'>";
  "dAs<b a=\"d'dd\" b='q&q\"qq'></q a=2></><a'a>" ==> "dAs<b a='d'dd' b='q&q\"qq'></q></><a>";
  "dsad<v" ==> "dsad<v>";
  "dsa" ==> "dsa";
  "" ==> "";
  "<" ==> "<>";
  "<a q=>" ==> "<a q=''>";
  "<a q='>" ==> "<a q='>'>";
  "<a b='&amp;'>&amp;</a>" ==> "<a b='&amp;'>&amp;</a>";
  "<a b='&'>&</a>" ==> "<a b='&'>&</a>";
end

let () = test "iequal" begin fun () ->
  let t = let n = ref 0 in fun x -> assert_bool (sprintf "testcase %d" !n) x; incr n in
  let fail = t $ not in
  t (Stre.iequal "dSaDAS" "dsadas");
  t (Stre.iequal "dsadas" "dsadas");
  t (Stre.iequal "../@423~|" "../@423~|");
  t (Stre.iequal "" "");
  t (Stre.iequal "привет" "привет");
  t (Stre.iequal "hello" "HELLO");
  fail (Stre.iequal "hello" "hello!");
  fail (Stre.iequal "hello1" "hello!");
end

let () = test "Nix.read_pidfile" begin fun () ->
  let f = ref "" in
  let pid = 42 in
  Control.with_open_out_temp_txt (fun (tmp, oc) -> f := tmp; Printf.fprintf oc "%d\n" pid);
  assert_equal (Nix.read_pidfile !f) pid
end

let () = test "Stre.iexists" begin fun () ->
  let f = Stre.iexists in
  let t = let n = ref 0 in fun x -> assert_bool (sprintf "testcase %d" !n) x; incr n in
  let fail = t $ not in
  t (f "xxxxdSaDAS" "dsadas");
  t (f "dSaDASxxxx" "dsadas");
  t (f "dSaDAS" "dsadas");
  t (f "xxxxdSaDASxxxx" "dsadas");
  t (f "xxxxdSaDAS" "DsAdAs");
  t (f "dSaDAS" "DsAdAs");
  t (f "xxxxdSaDASxxxx" "DsAdAs");
  t (f "dSaDASxxxx" "DsAdAs");
  t (f "xxxxdSaDAS" "");
  t (f "" "");
  t (f "12;dsaпривет" "привет");
  t (f "12;dsaпривет__324" "привет");
  fail (f "" "DsAdAs");
  fail (f "hello" "hellu");
  fail (f "hello" "hello!");
  fail (f "xxxxhello" "hello!");
  fail (f "helloxxx" "hello!");
  fail (f "hellox!helloXxx" "hello!");
  fail (f "" "x");
  fail (f "xyXZZx!x_" "xx");
end

let () = test "Cache.SizeLimited" begin fun () ->
  let module T = Cache.SizeLimited in
  let c = T.create 100 in
  let key = T.key in
  let test_get k = assert_equal ~printer:Std.dump (T.get c k) in
  let some k v = test_get (key k) (Some v) in
  let none k = test_get (key k) None in
  let iter f i1 i2 = for x = i1 to i2 do f x done in
  iter (fun i ->
    iter none i (1000+i);
    assert_equal (key i) (T.add c i);
    some i i) 0 99;
  iter (fun i ->
    iter none i (1000+i);
    iter (fun k -> some k k) (i-100) (i-1);
    assert_equal (key i) (T.add c i);
    some i i) 100 999;
  iter none 0 899;
  iter (fun k -> some k k) 900 999;
  iter none 1000 2000;
end

let () = test "Stre.splitc" begin fun () ->
  let t = assert_equal ~printer:(fun (a,b) -> sprintf "(%S,%S)" a b) in
  t ("a","b") (Stre.splitc "a.b" '.');
  t ("ab","") (Stre.splitc "ab." '.');
  t ("","ab") (Stre.splitc ".ab" '.');
  t ("","a.b") (Stre.splitc ".a.b" '.');
  t ("a","b") (Stre.rsplitc "a.b" '.');
  t ("ab","") (Stre.rsplitc "ab." '.');
  t ("","ab") (Stre.rsplitc ".ab" '.');
  t (".a","b") (Stre.rsplitc ".a.b" '.');
  t ("a.b","") (Stre.rsplitc "a.b." '.');
end

let () = test "Stre.before" begin fun () ->
  let t = assert_equal ~printer:id in
  t "" (Stre.before "abc" "");
  t "" (Stre.before "abc" "a");
  t "ab" (Stre.before "abc" "c");
  t "abc" (Stre.before "abc" "d");
  t "abc" (Stre.after "abc" "");
  t "bc" (Stre.after"abc" "a");
  t "" (Stre.after "abc" "c");
  t "" (Stre.after "abc" "d");
  let invariant s sub =
    let a = Stre.before s sub in
    let b = Stre.after s sub in
    t ~msg:(sprintf "invariant1 %S %S" s sub) s (a ^ (if String.exists s sub then sub else "")  ^ b);
    t ~msg:(sprintf "invariant2a %S %S" s sub) a (Stre.before (a ^ sub ^ b) sub);
    t ~msg:(sprintf "invariant2b %S %S" s sub) b (Stre.after (a ^ sub ^ b) sub);
    assert_equal ~msg:(sprintf "divide %S %S" s sub) (a,b) (Stre.divide s sub);
  in
  invariant "abc" "a";
  invariant "abc" "b";
  invariant "abc" "c";
  invariant "abc" "d";
  invariant "abc" "";
end

let () = test "Stre.by_words" begin fun () ->
  let t = let n = ref 0 in fun x -> assert_bool (sprintf "testcase %d" !n) x; incr n in
  let f a l = t (Stre.split Stre.by_words a = l) in
  f ("a" ^ String.make 10 '_' ^ "b") ["a"; "b"];
  f ("a" ^ String.make 1024 ' ' ^ "b") ["a"; "b"];
  f ("a" ^ String.make 10240 ' ' ^ "b") ["a"; "b"];
  f "проверка unicode!  www.ahrefs.com:работает " [ "проверка"; "unicode"; "www.ahrefs.com"; "работает"; ];
  let make sep n s = String.concat sep @@ List.make n s in
  f (make " " 10 @@ make "" 10240 "\239\191\189") [];
end

let () = test "ThreadPool" begin fun () ->
  let module TP = Parallel.ThreadPool in
  let pool = TP.create 3 in
  TP.wait_blocked pool;
  let i = ref 0 in
  for j = 1 to 10 do
    let worker _k () = incr i; Nix.sleep 0.2 in
    TP.put pool (worker j);
  done;
  TP.wait_blocked pool;
  assert_equal !i 10;
end

let () = test "Network.string_of_ipv4" begin fun () ->
  let t n s =
    assert_equal ~printer:Int32.to_string n (Network.int32_of_ipv4 @@ Network.ipv4_of_string_null s);
    assert_equal ~printer:id (Network.string_of_ipv4 @@ Network.ipv4_of_int32 n) s
  in
  t 0l "0.0.0.0";
  t 1l "0.0.0.1";
  t 16777216l "1.0.0.0";
  t 2130706433l "127.0.0.1";
  t 16777343l "1.0.0.127";
  t 0xFFFFFFFFl "255.255.255.255";
  t 257l "0.0.1.1"
end

let () = test "Network.ipv4_matches" begin fun () ->
  let t ip mask ok =
    try
      assert_equal ok (Network.ipv4_matches (Network.ipv4_of_string_null ip) (Network.cidr_of_string_exn mask))
    with
      _ -> assert_failure (Printf.sprintf "%s %s %B" ip mask ok)
  in
  t "127.0.0.1" "127.0.0.0/8" true;
  t "127.0.1.1" "127.0.0.0/8" true;
  t "128.0.0.1" "127.0.0.0/8" false;
  t "192.168.0.1" "192.168.0.0/16" true;
  t "192.168.1.0" "192.168.0.0/16" true;
  t "192.169.0.1" "192.168.0.0/16" false;
  t "0.0.0.0" "0.0.0.0/8" true;
  t "0.123.45.67" "0.0.0.0/8" true;
  t "10.0.0.1" "0.0.0.0/8" false;
  t "172.16.0.1" "172.16.0.0/12" true;
  t "172.20.10.1" "172.16.0.0/12" true;
  t "172.30.0.1" "172.16.0.0/12" true;
  t "172.32.0.1" "172.16.0.0/12" false;
  t "172.15.0.1" "172.16.0.0/12" false;
  t "172.1.0.1" "172.16.0.0/12" false;
  t "255.255.255.255" "255.255.255.255/32" true;
  t "255.255.255.254" "255.255.255.255/32" false
end

let () = test "Time.compact_duration" begin fun () ->
  let t n s =
    (* FIXME epsilon compare *)
    assert_equal ~printer:string_of_float n (Devkit_ragel.parse_compact_duration s);
  in
  let tt n s =
    t n s;
    assert_equal ~printer:id s (Time.compact_duration n);
  in
  tt 10. "10s";
  t 70. "70s";
  tt 70. "1m10s";
  t 10. "10";
  t 70. "70";
  t 70. "1m10";
  tt 61. "1m01s";
  t 61. "1m1s";
  tt 7200. "2h";
  t 7200. "2h0s";
  t 7200. "1h60m";
  t 7200. "1h60m0s";
  t 7200. "7200s";
  t 7200. "1h3600s";
  t 7200. "1h0m3600s";
  t 7200. "01h0m3600s";
  t 7200. "1h00m3600s";
  t 7200. "1h30m1800s";
  t 7201. "90m1801s";
  t 7201.01 "90m1801.01s";
  t 7201.1 "90m1801.1s";
  t 7200.1 "2h0.1s";
  t 0.8 "0.8s";
(*   tt 0.8 "800ms"; *)
  tt 5356800. "62d";
end

let () = test "Action.stable_partition" begin fun () ->
  let t l n result =
    let open Action in
    assert_equal ~msg:(sprintf "stable_partition %d" n) ~printer:(strl (strl string_of_int)) result (stable_partition n l)
  in
  t [] 0 [[]];
  t [] 1 [[]];
  t [] 2 [[];[]];
  t [] 3 [[];[];[]];
  t [1] 1 [[1]];
  t [1;2;3] 2 [[1;2];[3]];
  t [1;2;3;4;5;6;7;8] 3 [[1;2;3];[4;5;6];[7;8]];
  t [1;1;1;2;2;2;3;3;3;4;4;4;5;5;6;6] 6 [[1;1;1];[2;2;2];[3;3;3];[4;4;4];[5;5];[6;6]];
  let t l n =
    let open Action in
    assert_equal ~msg:(sprintf "stable_partition %d" n) ~printer:(strl string_of_int) l (stable_unpartition @@ stable_partition n l);
    assert_equal ~msg:(sprintf "partition %d" n) ~printer:(strl string_of_int) l (unpartition @@ partition n l)
  in
  t [1;2;3] 0;
  t [1;2;3] 1;
  t [] 0;
  t [] 1;
  for i = 1 to 10 do
    t (List.init (Random.int 10_000) id) (Random.int 100)
  done;
end

let () = test "Action.chunk" begin fun () ->
  let t l n result =
    let open Action in
    let printer = strl (strl string_of_int) in
    assert_equal ~msg:(sprintf "chunk %d" n) ~printer result (List.map List.rev @@ chunk n l);
    assert_equal ~msg:(sprintf "chunk_a %d" n) ~printer result (List.map Array.to_list @@ chunk_a n @@ Array.of_list l);
  in
  t [] 1 [];
  t [] 2 [];
  t [] 3 [];
  t [1] 1 [[1]];
  t [1;2;3] 2 [[1;2];[3]];
  t [1;2;3;4;5;6;7;8] 3 [[1;2;3];[4;5;6];[7;8]];
  t [1;2;3;4;5;6] 3 [[1;2;3];[4;5;6]];
  t [1;1;1;2;2;2;3;3;3;4;4;4;5;5;6;6] 6 [[1;1;1;2;2;2];[3;3;3;4;4;4];[5;5;6;6]];
end

let () = test "Enum.align" begin fun () ->
  let e1 = List.enum [1;3;6;] in
  let e2 = List.enum [2;4;5;7;8;] in
  let l = List.of_enum @@ Enum.align compare e1 e2 in
  let expect = [1;2;3;4;5;6;7;8;] in
  OUnit.assert_equal ~printer:(Action.strl string_of_int) expect l
end

let () = test "Enum.group_assoc" begin fun () ->
  OUnit.assert_equal ~msg:"1"
    ["ds", 3; "dsa", 7; "ds", 11; ]
    (List.of_enum @@ Enum.group_assoc (=) (+) 0 @@ List.enum ["ds",1; "ds",2; "dsa",3; "dsa",4; "ds", 1; "ds", 10]);
  OUnit.assert_equal ~msg:"2"
    []
    (List.of_enum @@ Enum.group_assoc (=) (+) 0 @@ List.enum []);
end

let () = test "Enum.join" @@ fun () ->
  let printer = Action.strl (uncurry @@ (sprintf "%s, %s" $$ Option.map_default string_of_int "NONE")) in
  let t ?left ?right ?multi e1 e2 expect =
    Enum.join ?left ?right ?multi compare (List.enum e1) (List.enum e2) |> List.of_enum |>
    OUnit.assert_equal ~printer expect
  in
  t ~left:true ~right:true [1;2;3;4;] [0;2;4;8;] [ None, Some 0; Some 1, None; Some 2, Some 2; Some 3, None; Some 4, Some 4; None, Some 8; ];
  t ~left:true ~right:true [0;1;2;3;] [4;5;6;7;] [ Some 0, None; Some 1, None; Some 2, None; Some 3, None; None, Some 4; None, Some 5; None, Some 6; None, Some 7; ];
  t ~left:true ~right:true [4;5;6;7;] [0;1;2;3;] [ None, Some 0; None, Some 1; None, Some 2; None, Some 3; Some 4, None; Some 5, None; Some 6, None; Some 7, None; ];
  t ~left:true ~right:true [0;1;2;3;] [0;3;] [ Some 0, Some 0; Some 1, None; Some 2, None; Some 3, Some 3; ];
  t ~left:true ~right:true [1;] [0;1;1;1;2;] [ None, Some 0; Some 1, Some 1; Some 1, Some 1; Some 1, Some 1; None, Some 2; ];
  t ~left:true ~right:true [1;] [0;1;1;] [ None, Some 0; Some 1, Some 1; Some 1, Some 1; ];
  t ~left:true ~right:true [1;2;] [0;1;1;] [ None, Some 0; Some 1, Some 1; Some 1, Some 1; Some 2, None; ];
  t ~left:true ~right:true ~multi:true [1;2;] [2;2;3;] [ Some 1, None; Some 2, Some 2; Some 2, Some 2; None, Some 3; ];
  t ~left:true ~right:true ~multi:false [1;2;] [2;2;3;] [ Some 1, None; Some 2, Some 2; None, Some 2; None, Some 3; ];
  ()

let () = test "Enum.uniq" begin fun () ->
  OUnit.assert_equal ~msg:"1" ~printer:(Action.strl id)
    ["ds"; "dsa"; "ds"; ]
    (List.of_enum @@ Enum.uniq (=) @@ List.enum ["ds"; "ds"; "dsa"; "dsa"; "ds"; "ds"]);
  OUnit.assert_equal ~msg:"2"
    []
    (List.of_enum @@ Enum.uniq (=) @@ List.enum []);
  OUnit.assert_equal ~msg:"3" ~printer:(Action.strl string_of_int)
    [1;20;1;2;3;44]
    (List.of_enum @@ Enum.uniq (fun x y -> x mod 10 = y mod 10) @@ List.enum [1;11;20;100;0;1;2;3;133;44]);
  OUnit.assert_equal ~msg:"4" ~printer:Std.dump
    [(1, 1); (2, 2); (3, 1); (1, 4); (4, 1)]
    (List.of_enum @@ Enum.count_unique (=) @@ List.enum [1;2;2;3;1;1;1;1;4;]);
end

let () = test "Enum.take" begin fun () ->
  let e = Enum.take 3 (Enum.empty ()) in
  OUnit.assert_bool "is_empty" (Enum.is_empty e);
  OUnit.assert_bool "peek None" (Enum.peek e = None);
  let e = Enum.take 3 (List.enum @@ List.init 10 id) in
  OUnit.assert_equal ~printer:(Stre.list string_of_int) [0;1;2] (List.of_enum e)
end

let () = test "Enum.iter_while" begin fun () ->
  let e = List.enum [1;2;3;4;5;6;7;] in
  Enum.iter_while (fun x -> if x = 2 then Enum.iter_while (fun x -> x < 6) e; x < 4) e;
  OUnit.assert_equal ~printer:(Action.strl string_of_int) [6;7] (List.of_enum e)
end

module LRU = struct
  module L = Cache.LRU(struct type t = int let equal = (=) let hash x = x end)
  open OUnit

  let () = test "LRU.put simple" begin fun () ->
      let size = 5 in
      let cache = L.create size in
      L.put cache 1 "a";
      assert_equal (L.lru_free cache) (size - 1);
      assert_equal (L.lfu_free cache) size;
      let value = L.get cache 1 in
      assert_equal ~msg:"insert_hashtbl" value "a";
      assert_equal (L.lru_free cache) size;
      assert_equal (L.lfu_free cache) (size - 1);
      L.put cache 1 "b";
      assert_equal (L.lru_free cache) size;
      assert_equal (L.lfu_free cache) (size - 1);
      assert_equal ~msg:"insert_hashtbl" (L.get cache 1) "b"
    end

  let () = test "LRU evict lru" begin fun () ->
      let size = 3 in
      let cache = L.create size in
      L.put cache 1 "a";
      L.put cache 2 "b";
      L.put cache 3 "c";
      assert_equal (L.lru_free cache) 0;
      assert_equal (L.lfu_free cache) 3;
      L.put cache 4 "c";
      assert_equal (L.lru_free cache) 0;
      assert_equal (L.lfu_free cache) 3;
      assert_equal ~printer:(function Some s -> sprintf "some %s" s | None -> "none") (try Some (L.get cache 1) with Not_found -> None) None;
      assert_bool "key 2" (L.mem cache 2);
      assert_bool "key 3" (L.mem cache 3);
      assert_bool "key 4" (L.mem cache 4);
    end

  let () = test "LRU evict lfu" begin fun () ->
      let size = 3 in
      let cache = L.create size in
      L.put cache 1 "a";
      L.put cache 2 "b";
      L.put cache 3 "c";
      assert_equal (L.get cache 1) "a";
      assert_equal (L.lru_free cache) 1;
      assert_equal (L.lfu_free cache) 2;
      L.put cache 4 "d";
      assert_equal (L.lru_free cache) 0;
      assert_equal (L.lfu_free cache) 2;
      let _ = L.get cache 2 in
      let _ = L.get cache 3 in
      assert_equal (L.lru_free cache) 2;
      assert_equal (L.lfu_free cache) 0;
      let _ = L.get cache 2 in
      assert_equal (L.lru_free cache) 2;
      assert_equal (L.lfu_free cache) 0;
      let _ = L.get cache 4 in
      assert_equal (L.lru_free cache) 3;
      assert_equal (L.lfu_free cache) 0;
      assert_bool "key 1 evicted" ( not @@ L.mem cache 1)
    end

  let () = test "LRU bounded" begin fun () ->
      let size = 2 in
      let cache = L.create size in
      for i = 0 to 20 do
        L.put cache i "a";
      done;
      assert_equal (L.lru_free cache) 0;
      assert_equal (L.lfu_free cache) 2;
      assert_equal (L.size cache) 2
    end

  let () = test "LFU keep" begin fun () ->
      let size = 2 in
      let cache = L.create size in
      L.put cache 1 "a";
      L.put cache 2 "b";
      let _ = L.get cache 2 in
      let _ = L.get cache 1 in
      for i = 3 to 6 do
        L.put cache i "b";
        (*let _ = L.get cache (i - 1) in*)
        let _ = L.get cache 1 in
        ()
      done;
      assert_equal (L.lru_free cache) 0;
      assert_equal (L.lfu_free cache) 0;
      assert_bool "key 1 kept" (L.mem cache 1)
    end

  let () = test "LFU remove" begin fun () ->
      let size = 2 in
      let cache = L.create size in
      L.put cache 1 "a";
      L.remove cache 1;
      assert_equal (L.lru_free cache) 2;
      assert_equal (L.lfu_free cache) 2;
      assert_bool "key 1 removed" (not @@ L.mem cache 1);
      L.put cache 2 "b";
      L.put cache 3 "c";
      let _ = L.get cache 2 in
      L.put cache 4 "d";
      L.remove cache 2;
      assert_equal (L.lfu_free cache) 2;
      assert_equal (L.lru_free cache) 0;
      assert_bool "key 2 removed" (not @@ L.mem cache 2);
      assert_bool "key 3 present" (L.mem cache 3)
    end
end

let tests () =
  let (_:test_results) = run_test_tt_main ("devkit" >::: List.rev !tests) in
  ()

let () =
  match Action.args with
  | ["http";port] -> Test_httpev.run (int_of_string port)
  | _ -> tests ()
