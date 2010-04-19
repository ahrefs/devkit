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
  Hashtbl.add values name (x:><gets:string;name:string>)

let reg_control (name:string) x =
  Hashtbl.add controls name (x:><gets:string; sets:string->bool>)

exception Unknown of string

let get name =
  try
    (Hashtbl.find values name) #gets
  with
    Not_found -> raise (Unknown name)

let set_control name x =
  try let c = Hashtbl.find controls name in c#sets x
  with _ -> false

let values () = Hashtbl.enum values >> Enum.map (fun (k,v) -> k,v#gets) >> List.of_enum >> 
  List.sort ~cmp:(fun (x,_) (y,_) -> compare x y)
let controls () = Hashtbl.enum controls >> Enum.map (fun (k,v) -> k,v#gets) >> List.of_enum >>
  List.sort ~cmp:(fun (x,_) (y,_) -> compare x y)

let value name =
object (self)

val mutable x = 0L
method inc = x <- Int64.succ x
method dec = x <- Int64.pred x
method addl n = x <- Int64.add x n
method add n = x <- Int64.add x (Int64.of_int n)
method get = x
method gets = Int64.to_string x
method name = name

initializer
  reg_value name self

end

let fvalue name =
object (self)

val mutable x = 0.
method add n = x <- x +. n
method gets = string_of_float x
method name = name

initializer
  reg_value name self

end

let time_value name =
object(self)
val mutable x = 0.
method add n = x <- x +. n
method gets = Time.duration_str x
method name = name
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

let fun_value name f =
object (self)

method gets = f ()
method name = name

initializer
  reg_value name self

end

let const name s =
object (self)

method gets = s
method name = name

initializer
  reg_value name self

end

let fun_time_value name f =
object (self)

method gets = Time.duration_str (f ())
method name = name

initializer
  reg_value name self

end

let store () = 
  let _vl = values () and cl = controls () in
  Marshal.to_string cl []

let restore s =
  let cl = (Marshal.from_string s 0 : (string*string) list) in
  List.iter (fun (k,v) -> ignore (set_control k v)) cl

open Printf
open Action

let cpu_time = fun_time_value "CPU time" Sys.time

let gc_heap = fun_value "Heap" begin fun () ->
    let st = Gc.quick_stat () in
    sprintf "%s (max %s)"
        (caml_words st.Gc.heap_words) 
        (caml_words st.Gc.top_heap_words)
    end

let gc_ctrs = fun_value "Counters(mi,pr,ma)" (fun () ->
    let st = Gc.quick_stat () in
    sprintf "%s %s %s"
        (caml_words_f st.Gc.minor_words)
        (caml_words_f st.Gc.promoted_words)
        (caml_words_f st.Gc.major_words))

let gc_coll = fun_value "Collections(mv,ma,mi)" (fun () ->
    let st = Gc.quick_stat () in
    sprintf "%u %u %u"
        st.Gc.compactions 
        st.Gc.major_collections 
        st.Gc.minor_collections)

let uptime =
  let start = Unix.time() in
  fun_time_value "Uptime" (fun () -> Unix.time() -. start)

let value_bytes name =
object (self)

val mutable x = 0L
method add n = x <- Int64.add x (Int64.of_int n)
method addl n = x <- Int64.add x n
method gets = Int64.to_float x >> bytes_string_f
method name = name

initializer
  reg_value name self

end

let gc = [ gc_heap; gc_ctrs; gc_coll; ]

let log x = Log.self #info "Varz %s : %s" x#name x#gets

let log_l l = Log.self #info "Varz %s" (String.concat " " (List.map (fun x -> sprintf "%s : %s" x#name x#gets) l))

