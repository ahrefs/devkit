open ExtLib
open Prelude

let state = Hashtbl.create 10

let escape k =
  if String.contains k ' ' then
    String.map (function ' ' -> '_' | c -> c) k
  else
    k

let zero = Var.(function Count _ -> Count 0 | Time _ -> Time 0. | Bytes _ -> Bytes 0)

let get () =
  let common =
    [
      "timestamp_ms", `Int (int_of_float @@ Time.now () *. 1000.);
      "pid", `String (Messaging.show_self ());
    ]
  in
  let system_memory =
    let gc = Gc.quick_stat () in
    let vm = Memory.get_vm_info () in
    let t = ("type", `String "system_memory") :: common in
    [
      `Assoc (("kind", `String "rss") :: ("bytes", `Int vm.rss) :: t);
      `Assoc (("kind", `String "vsz") :: ("bytes", `Int vm.vsize) :: t);
      `Assoc (("kind", `String "ocaml.heap") :: ("bytes",`Int (Action.bytes_of_words gc.heap_words)) :: t);
    ]
  in
  let l = ref system_memory in
  Var.iter begin fun t kname k v ->
    let (previous,t,k) =
      try Hashtbl.find state (t,k) with
      | Not_found -> let x = ref (zero v), escape t, escape k in Hashtbl.add state (t,k) x; x
    in
    match v, !previous with
    | Var.Count x, Count x' ->
      begin match x - x' with
      | 0 -> ()
      | delta -> previous := v; tuck l @@ `Assoc (("type",`String t) :: (kname, `String k) :: ("count", `Int delta) :: common)
      end
    | Bytes x, Bytes x' ->
      begin match x - x' with
      | 0 -> ()
      | delta -> previous := v; tuck l @@ `Assoc (("type",`String t) :: (kname, `String k) :: ("bytes", `Int delta) :: common)
      end
    | Time x, Time x' ->
      let delta = x -. x' in
      if delta > epsilon_float then
      begin
        previous := v;
        tuck l @@ `Assoc (("type",`String t) :: (kname, `String k) :: ("count", `Float delta) :: common)
      end
    | Count _, Bytes _ | Count _, Time _
    | Bytes _, Count _ | Bytes _, Time _
    | Time _, Count _ | Time _, Bytes _ -> () (* cannot happen *)
  end;
  !l
