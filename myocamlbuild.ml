open Ocamlbuild_plugin
open Command
open Printf
;;

let dversion =
  if Sys.ocaml_version >= "4.01" then [A"-DOCAML401"]
  else if Sys.ocaml_version >= "4.00" then [A"-DOCAML400"]
  else []
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

    flag ["ocaml"; "doc"] (S[A"-short-functors"; A"-sort"; A"-m"; A"A"; A"-hide-warnings"]);
    flag ["ocaml"; "pp"; "pa_macro"] (S[A"Camlp4MacroParser.cmo"]);
    flag ["ocaml"; "pp"; "dversion"] (S dversion);
    flag ["compile"; "ocaml"; "native"; "asm"] & atomize ["-S"];

    pflag ["compile"; "ocaml"] "warn" (fun s -> atomize ["-w";s]);

    ()
 
| _ -> ()
end
