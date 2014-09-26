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

let dyn_range ?(start=0) ?n d =
  let last =
    match n with
    | None -> DynArray.length d
    | Some n -> start + n
  in
  let rec make start =
    let idxref = ref start in
    let next () =
      if !idxref >= last then
        raise Enum.No_more_elements;
      let retval = DynArray.unsafe_get d !idxref in
      incr idxref;
      retval
    and count () =
      if !idxref >= last then 0 else last - !idxref
    and clone () =
      make !idxref
    in
    Enum.make ~next ~count ~clone
  in
  make start

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

let join ?(left=false) ?(right=false) ?(multi=true) f e1 e2 =
  let found = ref false in
  let rec next () =
    let found' = !found in
    found := false;
    match peek e1, peek e2 with
    | None, None -> raise No_more_elements
    | Some _, None as res -> junk e1; if left && not found' then res else next ()
    | None, Some _ as res -> junk e2; if right then res else raise No_more_elements
    | Some x, Some y as res ->
      match f x y with
      | n when n < 0 -> junk e1; if left && not found' then Some x, None else next ()
      | n when n > 0 -> junk e2; if right then None, Some y else next ()
      | _ -> if not multi then junk e1; junk e2; found := multi; res
  in
  from next

let merge f e1 e2 =
  let next () =
    match peek e1, peek e2 with
    | None, None -> raise No_more_elements
    | Some x, None -> junk e1; Some x, None
    | None, Some y -> junk e2; None, Some y
    | Some x, Some y ->
      match f x y with
      | 0 -> junk e1; junk e2; Some x, Some y
      | n when n < 0 -> junk e1; Some x, None
      | _ -> junk e2; None, Some y
  in
  from next

let merge_assoc f e1 e2 =
  let next () =
    match peek e1, peek e2 with
    | None, None -> raise No_more_elements
    | Some (k,x), None -> junk e1; k, Some x, None
    | None, Some (k,y) -> junk e2; k, None, Some y
    | Some (kx,x), Some (ky,y) ->
      match f kx ky with
      | 0 -> junk e1; junk e2; kx, Some x, Some y
      | n when n < 0 -> junk e1; kx, Some x, None
      | _ -> junk e2; ky, None, Some y
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

(** Replace subenum (a consecuitive sequence of elements from [e] comparing equal
  with the given comparison function) with the first element from that sequence and the number of duplicates *)
let count_unique equal e =
  let current = ref None in
  let n = ref 0 in
  let rec next () =
    match get e, !current with
    | None, None -> raise No_more_elements
    | None, Some x -> current := None; x, !n
    | Some v, None -> current := Some v; n := 1; next ()
    | Some v, Some x when equal x v -> incr n; next ()
    | Some v, Some x -> let count = !n in current := Some v; n := 1; x, count
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
