(** 
  Varz 

  TODO locking, grouping
*)

open ExtLib

open Prelude

(* module Value(T : Lock) = *)

type 'a t = < add : 'a -> unit; gets : string; name : string; >

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

val mutable x = 0
method inc = x <- succ x
method dec = x <- pred x
method add n = x <- x + n
method get = x
method gets = string_of_int x
method name = name

initializer
  reg_value name self

end

let fvalue name =
object (self)

val mutable x = 0.
method add n = x <- x +. n
method gets = string_of_float x
method get = x
method name = name

initializer
  reg_value name self

end

(** Exponential Weighted Moving Average 
  (smooth) 0.05 < alpha < 0.15 (dynamic)
*)
class ewma alpha name =
object (self)
val mutable x = nan
method add n = if compare nan x = 0 then x <- n else x <- x +. alpha *. (n -. x)
method gets = string_of_float x
method get = x
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
method add () = ()
method name = name

initializer
  reg_value name self

end

let const name s =
object (self)

method gets = s
method add () = ()
method name = name

initializer
  reg_value name self

end

let fun_time_value name f =
object (self)

method get = f ()
method gets = Time.duration_str (f ())
method add () = ()
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
    sprintf "%s (max %s, chunks %d)"
        (caml_words st.Gc.heap_words) 
        (caml_words st.Gc.top_heap_words)
        st.Gc.heap_chunks
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

val mutable x = 0
method add n = x <- x + n
method gets = bytes_string_f @@ float x
method get = x
method name = name

initializer
  reg_value name self

end

let gc = [ gc_heap; gc_ctrs; gc_coll; ]

let log x = Log.self #info "Varz %s : %s" x#name x#gets

let gets_l l = (String.concat " " (List.map (fun x -> sprintf "%s : %s" x#name x#gets) l))

let log_l l = Log.self #info "Varz %s" (gets_l l)

let gc_info () = gets_l gc

