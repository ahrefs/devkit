
open Printf

include Arg

let describe t name = function
  | None | Some "" -> sprintf "<%s> %s" t name
  | Some s when s.[0] = ' ' -> sprintf "<%s>%s" t s
  | Some s -> s

let arg_int name ?desc var =
  "-"^name, 
  Arg.Set_int var, 
  sprintf "%s (default: %i)" (describe "int" name desc) !var

let arg_str name ?desc var =
  "-"^name, 
  Arg.Set_string var, 
  sprintf "%s (default: %s)" (describe "string" name desc) !var

