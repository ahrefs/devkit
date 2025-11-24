open Devkit.Lurl

let print url = url |> to_string |> print_endline

open Printf

let print_result = function
  | Ok url -> printf "Ok %S\n%s\n" (to_string url) (debug url)
  | Error msg -> printf "Error %S\n" msg
let make_and_print ?scheme ~host ?port ?path ?args ?fragment () =
  make_args ?scheme ~host ?port ?path ?args ?fragment () |> to_string |> print_endline

let%expect_test "make" =
  make_and_print ~scheme:Https ~host:"example1.com" ~path:[ "foo"; "bar" ] ();
  make_and_print ~scheme:Https ~host:"example2.com" ~path:[ "foo"; "bar" ] ~args:[ "a", "b" ] ();
  make_and_print ~scheme:Https ~host:"example3.com" ~path:[ "foo"; "bar" ] ~args:[ "a b", "b c" ] ();
  make_and_print ~scheme:Https ~host:"example4.com" ~path:[ "foo"; "bar" ] ~args:[ "a+b", "b+c" ] ();
  make_and_print ~scheme:Https ~host:"example5.com" ~path:[ "foo"; "bar" ] ~args:[ "a,b", "b,c" ] ();
  make_and_print ~scheme:Http ~host:"example6.com" ~path:[ "foo/bar"; "baz" ] ();
  make_and_print ~host:"example7.com" ~path:[ "foo/bar"; "baz" ] ();
  make_and_print ~host:"122.140.201.66" ~port:8080 ();
  [%expect
    {|
    https://example1.com/foo/bar/
    https://example2.com/foo/bar/?a=b
    https://example3.com/foo/bar/?a%20b=b+c
    https://example4.com/foo/bar/?a%2Bb=b%2Bc
    https://example5.com/foo/bar/?a%2Cb=b%2Cc
    http://example6.com/foo%2Fbar/baz/
    http://example7.com/foo%2Fbar/baz/
    http://122.140.201.66:8080/|}]

let parse_and_print url = url |> parse |> print_result

let%expect_test "parse" =
  parse_and_print "ahrefs.com";
  [%expect {|
  Ok "http://ahrefs.com/"
  { Lurl.scheme = Lurl.Http; port = 80; path = []; query = []; fragment = "";
    host = "ahrefs.com" }
  |}];
  (* Supporting this is important because we have a lot of strings of this
    form in backend/apikit/upstreams/upstreams_gen.ml*)
  parse_and_print "122.140.201.66:8080";
  [%expect {|
  Ok "http://122.140.201.66:8080/"
  { Lurl.scheme = Lurl.Http; port = 8080; path = []; query = []; fragment = "";
    host = "122.140.201.66" }
  |}];
  parse_and_print "122.140.201.66:443";
  [%expect {|
  Ok "https://122.140.201.66/"
  { Lurl.scheme = Lurl.Https; port = 443; path = []; query = []; fragment = "";
    host = "122.140.201.66" }
  |}];
  parse_and_print "http://122.140.201.66:8080";
  [%expect {|
  Ok "http://122.140.201.66:8080/"
  { Lurl.scheme = Lurl.Http; port = 8080; path = []; query = []; fragment = "";
    host = "122.140.201.66" }
  |}];
  parse_and_print "https://ahrefs.com?k=v";
  [%expect {|
  Ok "https://ahrefs.com/?k=v"
  { Lurl.scheme = Lurl.Https; port = 443; path = []; query = [("k", ["v"])];
    fragment = ""; host = "ahrefs.com" }
  |}];
  parse_and_print "https://ahrefs.com?k=v1,v2";
  [%expect {|
  Ok "https://ahrefs.com/?k=v1,"
  { Lurl.scheme = Lurl.Https; port = 443; path = [];
    query = [("k", ["v1"; ""])]; fragment = ""; host = "ahrefs.com" }
  |}];
  parse_and_print "https://ahrefs.com?k=&k2=&k3=&v3=&=,,,";
  [%expect
    {|
    Ok "https://ahrefs.com/?k=&k2=&k3=&v3=&=,,,"
    { Lurl.scheme = Lurl.Https; port = 443; path = [];
      query =
      [("k", []); ("k2", [""]); ("k3", [""]); ("v3", [""]);
        ("", [""; ""; ""; ""])];
      fragment = ""; host = "ahrefs.com" } |}];
  parse_and_print "https://ahrefs.com#anchor";
  [%expect
    {|
    Ok "https://ahrefs.com/#anchor"
    { Lurl.scheme = Lurl.Https; port = 443; path = []; query = [];
      fragment = "anchor"; host = "ahrefs.com" } |}];
  parse_and_print "https://ahrefs.com/?k=v1#anchor";
  [%expect
    {|
    Ok "https://ahrefs.com/?k=v1#anchor"
    { Lurl.scheme = Lurl.Https; port = 443; path = []; query = [("k", ["v1"])];
      fragment = "anchor"; host = "ahrefs.com" } |}];
  parse_and_print "https://ahrefs%20.com/?a%20b+c=d%20e+f";
  [%expect
    {|
    Ok "https://ahrefs .com/?a%20b%2Bc=d+e+f"
    { Lurl.scheme = Lurl.Https; port = 443; path = [];
      query = [("a b+c", ["d e f"])]; fragment = ""; host = "ahrefs .com" } |}]


let%expect_test "make" =
  make_and_print ?scheme:None ~host:"ahrefs.com" ();
  [%expect {|http://ahrefs.com/|}]
