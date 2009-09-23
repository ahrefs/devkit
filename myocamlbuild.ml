open Ocamlbuild_plugin

;;

dispatch begin function
| After_rules ->

     flag ["ocaml"; "doc"; "use_extLib"] (S[A"-I"; A ("+extlib")]);
 
| _ -> ()
end
