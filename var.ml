open Printf
open ExtLib
open Prelude

let log = Log.from "var"

let show_a = Stre.list (uncurry @@ sprintf "%S:%S")

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
type group = { k : string; attr : Attr.t; mutable get : (unit -> (string * t option) list) list; }

let h_families = Hashtbl.create 10

let show_value = function
| Time t -> Time.compact_duration t
| Count c -> string_of_int c
| Bytes b -> Action.bytes_string b

let make_family ~name ~k ~attr =
  let family = Attr.make (("class",name)::attr) in
  let (_:Attr.t) = Attr.add (k,"") family in (* check that all keys are unique *)
  (k,family)

let register (k,family) get =
  match Hashtbl.find h_families family with
  | exception Not_found -> Hashtbl.replace h_families family { k; attr = family; get = [get] } (* register new family *)
  | r -> (* expand existing family *)
    log #warn "duplicate Var %s" (show_a @@ Attr.get family);
    r.get <- get :: r.get

let unregister (_k,family) = Hashtbl.remove h_families family

let is_in_families name =
  Hashtbl.fold begin fun k _ a ->
    match a with
    | true -> true
    | false -> List.exists (fun e -> e = ("class",name)) (Attr.get k)
  end h_families false

let make_cc f pp name ?(attr=[]) k =
  let cc = Cache.Count.create () in
  let get () = Cache.Count.fold cc (fun k n acc -> (pp k, f n) :: acc) [] in
  register (make_family ~name ~k ~attr) get;
  cc

let cc f = make_cc (fun n -> Some (Count n)) f
let cc_ms f = make_cc (fun n -> Some (Time (float n /. 1000.))) f

class typ name ?(attr=[]) k_name = (* name+attr - family of counters *)
let get_all h =
  Hashtbl.fold begin fun k v acc -> (* return all counters created for this instance *)
    match v () with
    | exception exn -> log #warn ~exn "variable %S %s failed" name (show_a @@ (k_name,k)::attr); acc
    | v -> (k, v) :: acc end h []
in
object(self)
  val h = Hashtbl.create 7
  val family = make_family ~k:k_name ~attr ~name
  initializer
    register family (fun () -> get_all h)
  method ref : 'a. 'a -> ('a -> t) -> string -> 'a ref = fun init f name ->
    let v = ref init in
    Hashtbl.replace h name (fun () -> some @@ f !v);
    v
  (* f() either returns Some value, either returns None, informing that value could not be obtained *)
  method get_count name f = Hashtbl.replace h name (fun () -> match f() with | Some x -> Some (Count x) | _ -> None)
  method get_bytes name f = Hashtbl.replace h name (fun () -> match f() with | Some x -> Some (Bytes x) | _ -> None)
  method get_time name f = Hashtbl.replace h name (fun () -> match f() with | Some x -> Some (Time x) | _ -> None)
  method count name = self#ref 0 (fun x -> Count x) name
  method bytes name = self#ref 0 (fun x -> Bytes x) name
  method time name = self#ref 0. (fun x -> Time x) name
  method unregister () = unregister family
  method get =
    get_all h |> List.filter_map (fun (k,v) -> match v with None -> None | Some v -> Some (k,v))
  method show =
    self#get |> List.map (fun (k,v) -> sprintf "%s: %s" k (show_value v)) |> String.concat ", "
end

let iter f =
  h_families |> Hashtbl.iter begin fun _name g -> (* iterate over counter families *)
    match g.get with
    | [get] -> (* no duplicates in this family *)
      get () |> List.iter begin fun (k,v) ->
        let attr = (g.k, k) :: Attr.get g.attr in (* this was checked to be valid in [register] *)
        match v with Some v -> f attr v | _ -> ()
      end
    | l -> (* list of getters for all instances created with this family name *)
      let h = Hashtbl.create 10 in
      l |> List.iter begin fun get ->
        get () |> List.iter begin fun (k, vl) -> (* merge values of duplicated counters in family *)
          match vl with
          | Some v ->
            let r = match Hashtbl.find h k with
              | exception Not_found -> Some v
              | Some x -> begin
                match x, v with
                | Time a, Time b -> Some (Time (a+.b))
                | Count a, Count b -> Some (Count (a+b))
                | Bytes a, Bytes b -> Some (Bytes (a+b))
                | Count _, Bytes _ | Count _, Time _
                | Bytes _, Count _ | Bytes _, Time _
                | Time _, Count _ | Time _, Bytes _ -> log #warn "mismatched value type for %S in %s" k (show_a @@ Attr.get g.attr); Some v
                end
              | None -> None
            in
            Hashtbl.replace h k r
          | None -> Hashtbl.replace h k None (* if at least one duplicate value is invalid - ignore all data for this counter *)
        end;
      end;
      h |> Hashtbl.iter begin fun k v ->
        let attr = (g.k, k) :: Attr.get g.attr in (* this was checked to be valid in [register] *)
        match v with Some v -> f attr v | _ -> ()
      end
  end

let list_stats filter =
  let l = ref [] in
  iter begin fun attrs v ->
    try
      let klass = List.assoc "class" attrs in
      if not @@ List.mem klass filter then raise Not_found; (* not interested stats *)
      let attrs = List.remove_assoc "class" attrs |> List.map (uncurry @@ sprintf "%s.%s") |> String.join "," in
      let value = show_value v in
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

(* non-monotonic, pointless to log*)
(* let system_memory = new typ "system_memory" "kind" *)
(* let () = system_memory#get_bytes "rss" (fun () -> (Memory.get_vm_info ()).rss) *)
(* let () = system_memory#get_bytes "vsize" (fun () -> (Memory.get_vm_info ()).vsize) *)
(* let () = system_memory#get_bytes "ocaml_heap" (fun () -> let gc = Gc.quick_stat () in Action.bytes_of_words gc.heap_words) *)
