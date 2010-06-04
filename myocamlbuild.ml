open Ocamlbuild_plugin
open Command
open Printf

(* let static = true *)

module C = Myocamlbuild_config

let () =
  let bracket res destroy k = let x = (try k res with e -> destroy res; raise e) in destroy res; x in
  let get_line r d = bracket r d input_line in

  bracket (open_out "version.ml") close_out (fun out ->
   let revision = 
    try
     get_line (Unix.open_process_in "svnversion") (Unix.close_process_in)
    with
     _ -> (try get_line (open_in "version.id") close_in with _ -> "<unknown>")
   in
   fprintf out "let id=\"%s\"\n" (String.escaped revision)
  )

;;

dispatch begin function
| Before_options ->

     if Sys.os_type = "Win32" then
     begin
       Options.ext_lib := "lib";
       Options.ext_obj := "obj";
       Options.ext_dll := "dll"
     end

| After_rules ->

    let copy_modules ext = rule ("copy devkit.modules to devkit."^ext) ~deps:["devkit.modules"] ~prod:("devkit."^ext)
      (fun _ _ -> Cmd (S[P"cp"; A"devkit.modules"; A("devkit."^ext)])) in

    copy_modules "mllib";
    copy_modules "odocl";
    copy_modules "mltop";

(*      PR#4873 *)
     flag ["thread"; "toplevel"; "link"; "ocaml"] (S[A"-thread";A"threads.cma"]);

     C.extern "extlib" ~cma:"extLib";
     C.extern "pcre";
     C.extern "netsys";
     C.extern "netstring";
     C.extern "equeue";
     C.extern "netclient";
     C.extern "netcgi2" ~cma:"netcgi";
     C.extern "oUnit";
     C.extern "zip";
     C.extern "json-wheel" ~cma:"jsonwheel";
     C.extern "sqlite3";
     C.extern "curl";
     C.extern "fileutils";
     C.extern "ocsigen.xhtml" ~cma:"xhtml";
     C.extern "ocsigen_xhtml" ~cma:"xhtml"; (* http://bugs.debian.org/579689 *)
     C.extern "event" ~cma:"liboevent";
(*
     C.extern "bin_prot";
     C.extern "sexplib";
     C.extern "core";
*)

     flag ["ocaml"; "doc"; "use_extLib"] (S[A"-I"; A (C.lib "extlib")]);
     flag ["ocaml"; "doc"; "use_netstring"] (S[A"-I"; A (C.lib "netstring")]);
     flag ["ocaml"; "doc"; "use_zip"] (S[A"-I"; A (C.lib "zip")]);
     flag ["ocaml"; "doc"; "use_pcre"] (S[A"-I"; A (C.lib "pcre")]);
     flag ["ocaml"; "doc"; "use_netcgi"] (S[A"-I"; A (C.lib "netcgi2")]);
     flag ["ocaml"; "doc"; "use_netsys"] (S[A"-I"; A (C.lib "netsys")]);
     flag ["ocaml"; "doc"; "use_xhtml"] (S[A"-I"; A (C.lib "ocsigen_xhtml")]);
     flag ["ocaml"; "doc"; "use_liboevent"] (S[A"-I"; A (C.lib "event")]);
     flag ["ocaml"; "doc"; "use_curl"] (S[A"-I"; A (C.lib "curl")]);

     flag ["ocaml"; "doc"] (S[A"-short-functors"; A"-sort"; A"-m"; A"A"]);

     flag ["ocaml"; "pp"; "use_json-static"] (A (Filename.concat (C.lib "json-static") "pa_json_static.cmo"));

     flag ["ocaml"; "pp"; "use_openin"] (A"pa_openin.cmo");
     flag ["ocaml"; "camlp4of"] (S[A"-I"; A"+camlp4"]);
     dep ["ocaml"; "ocamldep"; "use_openin"] ["pa_openin.cmo"];

     flag ["ocaml"; "pp"; "pa_macro"] (S[A"Camlp4MacroParser.cmo"]);

     (* If `static' is true then every ocaml link in bytecode will add -custom *)
(*      if static then flag ["link"; "ocaml"; "byte"] (A"-custom"); *)
 
     flag ["link"; "ocaml"; "use_netstring"; "thread"; "byte"] (S[A"netstring_mt.cmo"]);
     flag ["link"; "ocaml"; "use_netstring"; "thread"; "native"] (S[A"netstring_mt.cmx"]);
     flag ["link"; "ocaml"; "use_equeue"; "thread"; "byte"] (S[A"unixqueue_mt.cmo"]);
     flag ["link"; "ocaml"; "use_equeue"; "thread"; "native"] (S[A"unixqueue_mt.cmx"]);

     let stubs_lib = "libdevkit_stubs." ^ !Options.ext_lib in
     let stubs_dll = "dlldevkit_stubs." ^ !Options.ext_dll in
     flag ["link"; "ocaml"; "library"] (S[A"-cclib"; A stubs_lib;]);
     flag ["link"; "ocaml"; "library"; "byte"] (S[A"-dllib"; A stubs_dll;]);
     dep  ["link"; "ocaml"; "library"] [stubs_lib; stubs_dll];

     (* toplevel and tests use compiled devkit library *)
     ocaml_lib "devkit";
     flag ["link"; "ocaml"; "byte"; "use_devkit"] (atomize ["-dllpath"; "_build"]);

     ()
 
| _ -> ()
end

