open Ocamlbuild_plugin
open Command
open Printf

let () =
  let bracket res destroy k = let x = (try k res with e -> destroy res; raise e) in destroy res; x in
  let get_line r d = bracket r d input_line in

  bracket (open_out "version.ml") close_out (fun out ->
   let revision = 
    try
     get_line (Unix.open_process_in "git describe --always") (Unix.close_process_in)
    with
     _ -> (try get_line (open_in "version.id") close_in with _ -> "<unknown>")
   in
   fprintf out "let id=%S\n" revision
  )

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
    flag ["compile"; "ocaml"; "native"; "asm"] & atomize ["-S"];

    pflag ["compile"; "ocaml"] "warn" (fun s -> atomize ["-w";s]);

    ()
 
| _ -> ()
end
