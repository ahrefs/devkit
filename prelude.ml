(** Useful shortcuts *)

let ($) f g = fun x -> f (g x)
let ($$) f g = fun x y -> f (g x) (g y)
let (>>) x f = f x
let (&) f x = f x
let (!!) = Lazy.force

external id : 'a -> 'a = "%identity"
let flip f x y = f y x
let some x = Some x
let const x = fun () -> x

let apply2 f = fun (x,y) -> f x, f y
let printfn fmt = Printf.ksprintf print_endline fmt

let curry f a b = f (a, b)
let uncurry f (a,b) = f a b

(** exception for breaking from inner loops, MUST NOT leak outside *)
(* exception Break *)

module New(T : sig type t end) :
sig
  type t
  val inj : T.t -> t
  val proj : t -> T.t
end =
struct
  type t = T.t
  let inj = id
  let proj = id
end

let (+=) a b = a := !a + b

let fst3 (x,_,_) = x
let snd3 (_,x,_) = x
let trd3 (_,_,x) = x
