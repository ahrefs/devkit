
open OUnit
open Printf 

open Prelude

let test_search p s =
  let module WP = Web.Provider in
  let pr = print_endline in
  Enum.iter (fun (l,t,d) -> pr l; pr t; pr d; pr "") & p.WP.extract_full & Web.http_get & p.WP.request s

let test_htmlstream () =
  Printexc.record_backtrace true;
  let module HS = Web.HtmlStream in
  let (==>) s s' = 
  try
    let s'' = Control.wrapped_output (IO.output_string ()) (fun io -> ExtStream.iter (IO.nwrite io $ HS.show') (HS.parse (ExtStream.of_string s))) in
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
  "dAs<b a=\"d'dd\" b='q&q\"qq'></q a=2></><a'a>" ==> "dAs<b a='d&apos;dd' b='q&amp;q&quot;qq'></q></><a>";
  "dsad<v" ==> "dsad<v>";
  "dsa" ==> "dsa";
  "" ==> "";
  "<" ==> "<>";
  "<a q=>" ==> "<a q=''>";
  "<a q='>" ==> "<a q='>'>";
  ()

let test_iequal () =
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
  ()

let test_lim_cache () =
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
  ()

let tests () = 
  run_test_tt ("devkit" >::: [
    "HtmlStream" >:: test_htmlstream;
    "Stre.ieuqual" >:: test_iequal;
    "Cache.SizeLimited" >:: test_lim_cache;
  ]) >> ignore

let () =
  match Array.to_list Sys.argv with
  | [_;src;query] -> test_search (Web.Provider.by_name src) query
  | _ -> tests ()

