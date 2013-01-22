
exception Empty

type 'a t =
{
  next : (unit -> 'a);
  peek : (unit -> 'a);
  reset : (unit -> unit);
}

let make ~next ~peek = { next; peek; reset=(fun () -> ()); }
let from next =
  let cache = ref None in
  let peek () = match !cache with None -> let x = next () in cache := Some x; x | Some x -> x in
  let next () = match !cache with None -> next () | Some x -> cache := None; x in
  let reset () = cache := None in
  { next; peek; reset; }
let init n f = let i = ref 0 in from (fun () -> if !i >= n then raise Empty else incr i; f (!i - 1))
let empty = { next=(fun () -> raise Empty); peek=(fun () -> raise Empty); reset=(fun () -> ()); }

let next t = try Some (t.next ()) with Empty -> None
let peek t = try Some (t.peek ()) with Empty -> None
let next_exn t = t.next ()
let peek_exn t = t.peek ()
let junk t = ignore (next t)
let is_empty t = Option.is_none (peek t)
let reset t = t.reset ()

let iter f t = let rec loop () = match next t with Some x -> f x; loop () | None -> () in loop ()
let map f t = from (fun () -> f (next_exn t))
let filter_map f t =
  let rec loop () =
    match next t with
    | None -> raise Empty
    | Some x -> match f x with Some x -> x | None -> loop ()
  in
  from loop
