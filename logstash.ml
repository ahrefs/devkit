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
      "pid", `String (Pid.show_self ());
    ]
  in
  let l = ref [] in
  Var.iter begin fun attr v ->
    let (previous,attr) =
      try Hashtbl.find state attr with
      | Not_found -> let x = ref (zero v), List.map (fun (k, s) -> escape k, `String s) attr in Hashtbl.add state attr x; x
    in
    let this = List.append common attr in
    match v, !previous with
    | Count x, Count x' ->
      let delta = x - x' in
      if delta <> 0 then begin previous := v; tuck l @@ `Assoc (("count", `Int delta) :: this) end
    | Bytes x, Bytes x' ->
      let delta = x - x' in
      if delta <> 0 then begin previous := v; tuck l @@ `Assoc (("bytes", `Int delta) :: this) end
    | Time x, Time x' ->
      let delta = x -. x' in
      if delta > epsilon_float then begin previous := v; tuck l @@ `Assoc (("count", `Float delta) :: this) end
    | Count _, Bytes _ | Count _, Time _
    | Bytes _, Count _ | Bytes _, Time _
    | Time _, Count _ | Time _, Bytes _ -> () (* cannot happen *)
  end;
  !l
