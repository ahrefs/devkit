
open OUnit
open Printf 

open Prelude

let test_search p =
  let module WP = Web.Provider in
  let pr = print_endline in
  Enum.iter (fun (l,t,d) -> pr l; pr t; pr d; pr "") & p.WP.extract_full & Web.http_get & p.WP.request "cats"

let test_htmlstream () =
  Printexc.record_backtrace true;
  let module HS = Web.HtmlStream in
  let (==>) s s' = 
  try
    let s'' = Control.wrapped_output (IO.output_string ()) (fun io -> Stream.iter (IO.nwrite io $ HS.show') (HS.parse (Stream.of_string s))) in
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

let tests () = 
  run_test_tt ("devkit" >::: [
    "HtmlStream" >:: test_htmlstream;
    "Stre.ieuqual" >:: test_iequal;
  ]) >> ignore

let () =
  match List.tl & Array.to_list Sys.argv with
  | ["bing"] -> test_search Web.Provider.bing
  | ["google"] -> test_search Web.Provider.google
  | _ -> tests ()
