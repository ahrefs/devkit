open Printf
open ExtLib
open Prelude

let log = Log.from "var"

let show_a = Action.strl (uncurry @@ sprintf "%S:%S")

module Attr : sig
type t = private (string * string) list
val make : (string * string) list -> t
val add : (string * string) -> t -> t
val get : t -> (string * string) list
end = struct
type t = (string * string) list
let add (k,_ as x) l =
  if List.mem_assoc k l then Exn.fail "duplicate attribute %S" k;
  List.sort ~cmp:compare (x :: l)
let make l =
  let a = List.unique ~cmp:(fun (a,_) (b,_) -> a = b) l in
  if List.length a <> List.length l then Exn.fail "duplicate attributes : %s" (show_a l);
  List.sort ~cmp:compare l
let get = identity
end

type attributes = (string * string) list
type t = Time of Time.t | Count of int | Bytes of int
type group = { k : string; attr : Attr.t; mutable get : (unit -> (string * t) list) list; }

let h_groups = Hashtbl.create 10
let register ~name ~k ~get ~attr =
  let attr = Attr.make (("class",name)::attr) in
  let (_:Attr.t) = Attr.add (k,"") attr in (* check that all keys are unique *)
  match Hashtbl.find h_groups attr with
  | exception Not_found -> Hashtbl.replace h_groups attr { k; attr; get = [get] }
  | r ->
    log #warn "duplicate Var %s" (show_a @@ Attr.get attr);
    r.get <- get :: r.get

let make_cc f pp name ?(attr=[]) k =
  let cc = Cache.Count.create () in
  let get () = Cache.Count.fold cc (fun k n acc -> (pp k, f n) :: acc) [] in
  register ~name ~k ~get ~attr;
  cc

let cc f = make_cc (fun n -> Count n) f
let cc_ms f = make_cc (fun n -> Time (float n /. 1000.)) f

class typ name ?(attr=[]) k_name =
object(self)
  val h = Hashtbl.create 7
  initializer
    let get () = Hashtbl.fold (fun k v acc ->
      match v () with
      | exception exn -> log #warn ~exn "variable %S %s failed" name (show_a @@ (k_name,k)::attr); acc
      | v -> (k, v) :: acc) h [] in
    register ~k:k_name ~get ~attr ~name
  method ref : 'a. 'a -> ('a -> t) -> string -> 'a ref = fun init f name ->
    let v = ref init in
    Hashtbl.replace h name (fun () -> f !v);
    v
  method get_count name f = Hashtbl.replace h name (fun () -> Count (f ()))
  method get_bytes name f = Hashtbl.replace h name (fun () -> Bytes (f ()))
  method get_time name f = Hashtbl.replace h name (fun () -> Time (f ()))
  method count name = self#ref 0 (fun x -> Count x) name
  method bytes name = self#ref 0 (fun x -> Bytes x) name
  method time name = self#ref 0. (fun x -> Time x) name
end

let iter f =
  h_groups |> Hashtbl.iter begin fun _name g ->
    match g.get with
    | [get] ->
      get () |> List.iter begin fun (k,v) ->
        let attr = (g.k, k) :: Attr.get g.attr in (* this was checked to be valid in [register] *)
        f attr v
      end
    | l ->
      (* uh *)
      let h = Hashtbl.create 10 in
      l |> List.iter begin fun get ->
        get () |> List.iter begin fun (k,v) ->
          let r = match Hashtbl.find h k with
          | exception Not_found -> v
          | x ->
            match x, v with
            | Time a, Time b -> Time (a+.b)
            | Count a, Count b -> Count (a+b)
            | Bytes a, Bytes b -> Bytes (a+b)
            | Count _, Bytes _ | Count _, Time _
            | Bytes _, Count _ | Bytes _, Time _
            | Time _, Count _ | Time _, Bytes _ -> log #warn "mismatched value type for %S in %s" k (show_a @@ Attr.get g.attr); v
          in
          Hashtbl.replace h k r
        end;
      end;
      h |> Hashtbl.iter begin fun k v ->
        let attr = (g.k, k) :: Attr.get g.attr in (* this was checked to be valid in [register] *)
        f attr v
      end
  end

let list_stats filter =
  let l = ref [] in
  iter begin fun attrs v ->
    try
      let klass = List.assoc "class" attrs in
      if not @@ List.mem klass filter then raise Not_found; (* not interested stats *)
      let attrs = List.remove_assoc "class" attrs |> List.map (uncurry @@ sprintf "%s.%s") |> String.join "," in
      let value =
        match v with
        | Time t -> Time.compact_duration t
        | Count c -> string_of_int c
        | Bytes b -> Action.bytes_string b
      in
      tuck l @@ sprintf "%s %s : %s" klass attrs value
    with Not_found -> ()
  end;
  List.sort !l

(*
let show () =
  let b = Buffer.create (Hashtbl.length h_vars * 20) in
  iter begin fun ~t ~k ~kname:_ ~v ->
    Printf.bprintf b "%s[%s]=%s " t k (match v with Int n -> string_of_int n | Float f -> string_of_float f);
  end;
  Buffer.contents b
*)

let system_memory = new typ "system_memory" "kind"

let () = system_memory#get_bytes "rss" (fun () -> (Memory.get_vm_info ()).rss)
let () = system_memory#get_bytes "vsize" (fun () -> (Memory.get_vm_info ()).vsize)
let () = system_memory#get_bytes "ocaml_heap" (fun () -> let gc = Gc.quick_stat () in Action.bytes_of_words gc.heap_words)
