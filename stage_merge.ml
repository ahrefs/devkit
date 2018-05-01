open Enum

type ('a,'b) value = Left of 'a | Right of 'b | Both of ('a * 'b)

let stage_merge compare ~left ~right ~multi return key1 key2 =
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
      | Some x, None -> junk e1; .~(let ret = return (key1 .<x>.) (Left .<x>.) in multi_ret .<next>. .<_prev_found>. ret)
      | None, Some y -> junk e2; .~(if right then return (key2 .<y>.) (Right .<y>.) else .< raise No_more_elements >.)
      | Some x, Some y ->
        let k1 = .~(key1 .<x>.) in
        let k2 = .~(key2 .<y>.) in
        match .~compare k1 k2 with
        | 0 -> .~(if not multi then .< junk e1 >. else if left then .< _found := true >. else .< () >.); junk e2; .~(return .<k1>. (Both (.<x>.,.<y>.)))
        | n when n < 0 -> junk e1; .~(let ret = return .<k1>. (Left .<x>.) in multi_ret .<next>. .<_prev_found>. ret)
        | _ (* n > 0 *) -> junk e2; .~(if right then return .<k2>. (Right .<y>.) else .< next () >.)
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
  Print_code.format_code std_formatter (Print_code.close_code code);
  pp_print_newline std_formatter ();
  pp_print_newline std_formatter ()

(* generate *)

let ret_pair a b = (fun _k v -> match v with Left x -> .< .~(a x), None >. | Right x -> .< None, .~(b x) >. | Both (x,y) -> .<.~(a x), .~(b y) >.)
let ret_assoc a b = (fun k v -> match v with Left x -> .<.~k, .~(a x), None>. | Right x -> .<.~k, None, .~(b x)>. | Both (x,y) -> .<.~k, .~(a x), .~(b y)>.)
let ret_full a b = (fun _k v -> match v with Left x -> .< `Left .~(a x) >. | Right x -> .< `Right .~(b x) >. | Both (x,y) -> .< `Both (.~(a x), .~(b y)) >.)
let ret_add_key f = (fun k v -> .< .~k, .~(f k v) >.)

let () =
  let bool k = k false; k true in
  bool @@ fun assoc -> bool @@ fun uniq -> bool @@ fun right -> bool @@ fun left -> bool @@ fun by ->
    match by, assoc with
    | true, true -> ()
    | _ ->
    let dir =
      match left, right with
      | true, true -> "full"
      | true, false -> "left"
      | false, true -> "right"
      | false, false -> "inner"
    in
    let str b name = if b then name else "" in
    let name = String.concat "_" @@ List.filter ((<>) "") @@ ["join"; str assoc "assoc"; dir; str uniq "uniq"; str by "by"] in
    Printf.printf "let %s =\n" name;
    let gen cmp ret k1 k2 = stage_merge cmp ~left ~right ~multi:(not uniq) ret k1 k2 in
    let gen ret key =
      if by then
        print_code .< fun cmp k1 k2 -> .~(gen .<cmp>. ret (fun x -> .<k1 .~x>.) (fun x -> .<k2 .~x>.)) >.
      else
        print_code @@ .< fun cmp -> .~(gen .<cmp>. ret key key) >.
    in
    let gen v1 v2 =
      match left && right, assoc with
      | false, false -> gen (ret_pair v1 v2) id
      | false, true -> gen (ret_assoc (v1 $ snd_) (v2 $ snd_)) fst_
      | true, false -> gen (ret_full v1 v2) id
      | true, true -> gen (ret_add_key @@ ret_full (v1 $ snd_) (v2 $ snd_)) fst_
    in
    begin match left, right with
    | true, true -> gen id id
    | true, false -> gen id some
    | false, true -> gen some id
    | false, false -> gen id id
    end;
    if by then Printf.printf "let %s_key cmp k = %s cmp k k\n\n" name name

let stage_full_merge return key = .< fun cmp -> .~(stage_merge .<cmp>. ~left:true ~right:true ~multi:false return key key) >.

let () =
  print_endline "let merge =";
  print_code @@ stage_full_merge (ret_pair some some) id

let () =
  print_endline "let merge_assoc =";
  print_code @@ stage_full_merge (ret_assoc (some $ snd_) (some $ snd_)) fst_

(*
let () =
  print_endline "let merge_by =";
  print_code @@ .< fun compare key1 key2 -> .~(stage_full_merge (ret_pair some some) (fun x -> .<key1 .~x>.) (fun x -> .<key2 .~x>.)) compare >.
*)
