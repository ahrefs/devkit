open Ocamlbuild_plugin
open Command
open Printf
;;

dispatch begin function
| After_rules ->

    let copy_modules ext = rule ("copy devkit.modules to devkit."^ext) ~deps:["devkit.modules"] ~prod:("devkit."^ext)
      (fun _ _ -> Cmd (S[P"cp"; A"devkit.modules"; A("devkit."^ext)]))
    in

    copy_modules "mllib";
    copy_modules "odocl";
    copy_modules "mltop";

    flag ["thread"; "doc"; "ocaml"] (S[A"-thread"]);
    flag ["thread"; "link"; "ocaml"] (S[A"-thread"]);

    flag ["ocaml"; "infer_interface"; "syntax_camlp4o"] (S[A"-syntax"; A"camlp4o"]);
    flag ["ocaml"; "compile"; "syntax_camlp4o"] (S[A"-syntax"; A"camlp4o"]);
    flag ["ocaml"; "ocamldep"; "syntax_camlp4o"] (S[A"-syntax"; A"camlp4o"]);
    flag ["ocaml"; "doc"; "syntax_camlp4o"] (S[A"-syntax"; A"camlp4o"]);

    flag ["ocaml"; "doc"] (S[A"-short-functors"; A"-sort"; A"-m"; A"A"; A"-hide-warnings"]);
    flag ["compile"; "ocaml"; "native"; "asm"] & atomize ["-S"];

    ()

| _ -> ()
end
