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

let align f e1 e2 =
  let next () =
    match peek e1, peek e2 with
    | None, None -> raise No_more_elements
    | Some x, None -> junk e1; x
    | None, Some y -> junk e2; y
    | Some x, Some y when f x y < 0 -> junk e1; x
    | Some _, Some y -> junk e2; y
  in
  from next

let group equal fold zero e =
  let current = ref None in
  let rec next () =
    match get e, !current with
    | None, None -> raise No_more_elements
    | None, Some x -> current := None; x
    | Some v, None -> current := Some (fold zero v); next ()
    | Some v, Some x when equal x v -> current := Some (fold x v); next ()
    | Some v, Some x -> current := Some (fold zero v); x
  in
  from next

let group_assoc equal fold zero e =
  let current = ref None in
  let rec next () =
    match get e, !current with
    | None, None -> raise No_more_elements
    | None, Some x -> current := None; x
    | Some (k,v), None -> current := Some (k, fold zero v); next ()
    | Some (k,v), Some (g,acc) when equal k g -> current := Some (g, fold acc v); next ()
    | Some (k,v), Some cur -> current := Some (k, fold zero v); cur
  in
  from next

(** Replace subenum (a consecuitive sequence of elements from [e] comparing equal
  with the given comparison function) with the first element from that sequence *)
let uniq equal e =
  let current = ref None in
  let rec next () =
    match get e, !current with
    | None, None -> raise No_more_elements
    | None, Some x -> current := None; x
    | Some v, None -> current := Some v; next ()
    | Some v, Some x when equal x v -> next ()
    | Some v, Some x -> current := Some v; x
  in
  from next

(** Extract subenum (a consecuitive sequence of the elements from [e])
  that satisfy the predicate [f] *)
let sub e f =
  match peek e with
  | None -> None
  | Some x ->
    let current = f x in
    let next () =
      match peek e with
      | Some x when f x = current -> junk e; x
      | None | Some _ -> raise No_more_elements
    in
    Some (current, from next)

let rec iter_while f e =
  match peek e with
  | Some x when f x ->
    begin match peek e with
    | Some y when x == y -> junk e (* "support" recursive invocations *)
    | _ -> ()
    end;
    iter_while f e
  | _ -> ()
