open Prelude

exception Exit

(* same as Enum.find, but found element is peeked, but not junked *)
let rec find_peek f e =
  match Enum.peek e with
  | Some x when f x ->
    x
  | None ->
    raise Not_found
  | _ ->
    Enum.junk e;
    find_peek f e

let list_loop l =
  assert (l <> []);
  let r = ref l in
  let rec next () =
    match !r with
    | x :: xs -> r := xs; x
    | [] -> r := l; next ()
  in
  Enum.from next

(* same as list loop, but after loop takes new list *)
let list_loop_changes l =
  assert (!l <> []);
  let r = ref !l in
  let rec next () =
    match !r with
    | x :: xs -> r := xs; x
    | [] -> assert (!l <> []); r := !l; next ()
  in
  Enum.from next

(* HACK *)
let next : 'a Enum.t -> 'a = fun e -> (Obj.obj (Obj.field (Obj.repr e) 1) : unit -> 'a) ()
let take limit e = Enum.init limit (fun _ -> next e)
