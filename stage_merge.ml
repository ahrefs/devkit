open Enum

type ('a,'b) value = Left of 'a | Right of 'b | Both of ('a * 'b)

let stage_merge compare ~left ~right ~multi return key1 key2 v1 v2 =
  let multi_ret next found ret =
    match left, multi with
    | true, true -> .< if not .~found then .~ret else .~next () >.
    | true, false -> ret
    | false, _ -> .< .~next () >.
  in
  .< fun e1 e2 ->
    let _found = ref false in
    let rec next () =
      let _prev_found = .~(if multi && left then .< let prev = !_found in _found := false; prev>.  else .< false >.) in
      match peek e1, peek e2 with
      | None, None -> raise No_more_elements
      | Some x, None -> junk e1; .~(let ret = return (key1 .<x>.) (Left (v1 .<x>.)) in multi_ret .<next>. .<_prev_found>. ret)
      | None, Some y -> junk e2; .~(if right then return (key2 .<y>.) (Right (v2 .<y>.)) else .< raise No_more_elements >.)
      | Some x, Some y ->
        let k1 = .~(key1 .<x>.) in
        let k2 = .~(key2 .<y>.) in
        match .~compare k1 k2 with
        | 0 -> .~(if not multi then .< junk e1 >. else if left then .< _found := true >. else .< () >.); junk e2; .~(return .<k1>. (Both (v1 .<x>., v2 .<y>.)))
        | n when n < 0 -> junk e1; .~(let ret = return .<k1>. (Left (v1 .<x>.)) in multi_ret .<next>. .<_prev_found>. ret)
        | _ (* n > 0 *) -> junk e2; .~(if right then return .<k2>. (Right (v2 .<y>.)) else .< next () >.)
    in
    from next
  >.

(* helpers *)

let lift f x = .<f .~x>. (* csp *)
let fst_ x = .<fst .~x>.
let snd_ x = .<snd .~x>.
let some x = .<Some .~x>.

let id x = x
let same f x = f x x
let ($) f g = fun x -> f @@ g x

let print_code code =
  let open Format in
  format_code std_formatter (close_code code);
  pp_print_newline std_formatter ();
  pp_print_newline std_formatter ()

(* generate *)

let wrap ret v1 v2 =
  fun k v ->
    let v = match v with Left x -> Left (v1 x) | Right x -> Right (v2 x) | Both (x,y) -> Both (v1 x, v2 y) in
    ret k v

let ret_pair _k v = match v with Left x -> .< .~x, None >. | Right x -> .< None, .~x >. | Both (x,y) -> .<.~x, .~y >.
let ret_assoc k v = match v with Left x -> .<.~k, .~x, None>. | Right x -> .<.~k, None, .~x>. | Both (x,y) -> .<.~k, .~x, .~y>.
let ret_full _k v = match v with Left x -> .< `Left .~x >. | Right x -> .< `Right .~x >. | Both (x,y) -> .< `Both (.~x, .~y) >.
let ret_add_key f k v = .< .~k, .~(f k v) >.

let () =
  let bool k = k false; k true in
  bool @@ fun assoc -> bool @@ fun multi -> bool @@ fun right -> bool @@ fun left -> bool @@ fun by ->
    match by, assoc with
    | true, true -> () (* assoc doesn't need `by`, has explicit key already *)
    | false, false -> () (* we don't want non-`by` variants, except for assoc which has explicit key *)
    | _ ->
    let dir =
      match left, right with
      | true, true -> "full"
      | true, false -> "left"
      | false, true -> "right"
      | false, false -> "inner"
    in
    let str b name = if b then name else "" in
    let name = String.concat "_" @@ List.filter ((<>) "") @@ ["join"; str assoc "assoc"; dir; str multi "multi"; str by "by"] in
    Printf.printf "let %s =\n" name;
    let stage cmp ret k1 k2 v = stage_merge cmp ~left ~right ~multi ret k1 k2 v v in
    let gen key v ret =
      if by then
        print_code .< fun cmp k1 k2 -> .~(stage .<cmp>. ret (fun x -> .<k1 .~x>.) (fun x -> .<k2 .~x>.) v) >.
      else
        print_code .< fun cmp -> .~(stage .<cmp>. ret key key v) >.
    in
    let gen v1 v2 =
      match assoc, left && right with
      | false, false -> gen id id (wrap ret_pair v1 v2)
      | false, true -> gen id id ret_full
      | true, false -> gen fst_ snd_ (wrap ret_assoc v1 v2)
      | true, true -> gen fst_ snd_ (ret_add_key @@ ret_full)
    in
    begin match left, right with
    | true, true -> gen id id
    | true, false -> gen id some
    | false, true -> gen some id
    | false, false -> gen id id
    end;
    if by then Printf.printf "let %s_key cmp k = %s cmp k k\n\n" name name

let stage_full_merge return key v = .< fun cmp -> .~(stage_merge .<cmp>. ~left:true ~right:true ~multi:false return key key v v) >.

let () =
  print_endline "let merge =";
  print_code @@ stage_full_merge (wrap ret_pair some some) id id

let () =
  print_endline "let merge_assoc =";
  print_code @@ stage_full_merge (wrap ret_assoc some some) fst_ snd_

(*
let () =
  print_endline "let merge_by =";
  print_code @@ .< fun compare key1 key2 -> .~(stage_full_merge (ret_pair some some) (fun x -> .<key1 .~x>.) (fun x -> .<key2 .~x>.)) compare >.
*)
