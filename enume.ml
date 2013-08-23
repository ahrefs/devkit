open Prelude

exception Exit

(* same as Enum.find, but found element is peeked, but not junked *)
let find_peek f e =
  try
    while true do
      if not & f & Option.get & Enum.peek e then Enum.junk e else raise Exit
    done;
    assert false (* unreachable *)
  with
  | Exit -> Option.get & Enum.peek e
  | Option.No_value -> raise Not_found
  | exn -> raise exn (* f *)

let list_loop l =
  assert (l <> []);
  let r = ref l in
  let rec next () =
    match !r with
    | x :: xs -> r := xs; x
    | [] -> r := l; next ()
  in
  Enum.from next
