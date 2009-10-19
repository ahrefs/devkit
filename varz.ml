(** 
  Varz 

  TODO locking, grouping
*)

open ExtLib

open Prelude

(* module Value(T : Lock) = *)

let values = Hashtbl.create 16
let controls = Hashtbl.create 16

let reg_value (name:string) x =
  Hashtbl.add values name (x:><gets:string>)

let reg_control (name:string) x =
  Hashtbl.add controls name (x:><gets:string; sets:string->bool>)

let set_control name x =
  try let c = Hashtbl.find controls name in c#sets x
  with _ -> false

let values () = Hashtbl.enum values >> Enum.map (fun (k,v) -> k,v#gets)
let controls () = Hashtbl.enum controls >> Enum.map (fun (k,v) -> k,v#gets)

let value name =
object (self)

val mutable x = 0L
method inc = x <- Int64.succ x
method addl n = x <- Int64.add x n
method add n = x <- Int64.add x (Int64.of_int n)
method gets = Int64.to_string x

initializer
  reg_value name self

end

let fvalue name =
object (self)

val mutable x = 0.
method add n = x <- x +. n
method gets = string_of_float x

initializer
  reg_value name self

end

let set_int name init = object(self)

val mutable v = init
method sets x = try v <- int_of_string x; true with _ -> false
method gets = string_of_int v
method get = v

initializer
  reg_control name self

end

let store () = 
  let _vl = values () >> List.of_enum and cl = controls () >> List.of_enum in
  Marshal.to_string cl []

let restore s =
  let cl = (Marshal.from_string s 0 : (string*string) list) in
  List.iter (fun (k,v) -> ignore (set_control k v)) cl

