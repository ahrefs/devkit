(** Extensions to Enum *)

include Enum

(* same as Enum.find, but found element is peeked, not junked *)
let rec find_peek f e =
  match peek e with
  | Some x when f x ->
    x
  | None ->
    raise Not_found
  | _ ->
    junk e;
    find_peek f e

let list_loop l =
  assert (l <> []);
  let r = ref l in
  let rec next () =
    match !r with
    | x :: xs -> r := xs; x
    | [] -> r := l; next ()
  in
  from next

(* same as list loop, but after loop takes new list *)
let list_loop_changes l =
  assert (!l <> []);
  let r = ref !l in
  let rec next () =
    match !r with
    | x :: xs -> r := xs; x
    | [] -> assert (!l <> []); r := !l; next ()
  in
  from next

let take limit e = init limit (fun _ -> next e)
