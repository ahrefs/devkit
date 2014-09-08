
open Printf

open Prelude

include Arg

let describe t name = function
  | "" -> sprintf "<%s> %s" t name
  | s when s.[0] = ' ' -> sprintf "<%s>%s" t s
  | s -> s

let make_arg x = 
  fun name var desc ->
  "-"^name,
  x#store var,
  sprintf "%s (default: %s)" (describe x#kind name desc) (x#show var)

let test_int f = object
method store v = Arg.Int (fun x -> if not (f x) then Exn.fail "Bad value %d" x; v := x)
method kind = "int"
method show v = string_of_int !v
end

let int = object
method store v = Arg.Set_int v
method kind = "int"
method show v = string_of_int !v
end

let string = object
method store v = Arg.Set_string v
method kind = "string"
method show v = !v
end

let duration = object
method store v = Arg.String (fun s -> v := Time.of_compact_duration s)
method kind = "duration"
method show v = Time.compact_duration !v
end

let int_option = object
method store v = Arg.Int (fun x -> v := Some x)
method kind = "int"
method show v = Option.map_default string_of_int "none" !v
end

let str_option = object
method store v = Arg.String (fun x -> v := Some x)
method kind = "string"
method show v = Option.map_default id "none" !v
end

let int = make_arg int
let str = make_arg string
let duration = make_arg duration
let may_int = make_arg int_option
let may_str = make_arg str_option
let positive_int = make_arg (test_int (fun x -> x > 0))

let usage_header = sprintf "Usage: %s [options]\nOptions are:" Sys.argv.(0)

let align ?(sep="#") args =
  let open ExtString in
  let convert ~sub ~by (a, b, doc) =
    let (doc:doc) =
      try
        if doc = "" || doc.[0] = ' ' then doc else
        let (left, right) = String.split doc by in
        (Stre.replace_all ~str:left ~sub ~by) ^ " " ^ right
      with Invalid_string -> doc
    in
    (a, b, doc)
  in
  args |>
  List.map (convert ~sub:" " ~by:sep) |>
  align |>
  List.map (convert ~sub:sep ~by:" ")

let parse ?f args =
  let f = Option.default (fun s -> Exn.fail "unrecognized argument %S, try \"-help\"" s) f in
  parse (align args) f usage_header

let usage args = Arg.usage (align args) usage_header

(*
  "-"^name, 
  Arg.Set_int var, 
  sprintf "%s (default: %i)" (describe "int" name desc) !var
*)

(*
let arg_str name ?desc var =
  "-"^name, 
  Arg.Set_string var, 
  sprintf "%s (default: %s)" (describe "string" name desc) !var
*)

let two_strings k =
  (let old = ref "" in
    Arg.Tuple [
       Arg.String (fun x -> old := x);
       Arg.String (fun s -> k !old s)
      ])

let rest () =
  let n = Array.length Sys.argv in
  if !Arg.current >= n then
    []
  else
    Array.to_list @@ Array.sub Sys.argv (!Arg.current+1) (Array.length Sys.argv - !Arg.current - 1)
