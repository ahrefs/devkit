(** Useful shortcuts *)

let ($) f g = fun x -> f (g x)
let (>>) x f = f x
let (&) f x = f x

external id : 'a -> 'a = "%identity"
let flip f x y = f y x
let some x = Some x
let const x = fun () -> x

let apply2 f = fun (x,y) -> f x, f y
(* let println = print_endline *)

