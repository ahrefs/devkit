(** Useful shortcuts *)

module U = ExtUnix.Specific
module Enum = ExtEnum

let ($) f g = fun x -> f (g x)
let ($$) f g = fun x y -> f (g x) (g y)
let (!!) = Lazy.force

(** @deprecated in favor of [|>] *)
let (>>) x f = f x
(** @deprecated in favor of [\@\@] *)
let (&) f x = f x
(** reverse apply : [x |> f |> g] is equivalent to [g (f x)] *)
let (|>) x f = f x
(* external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply" *)
(** apply : [g \@\@ f \@\@ x] is equivalent to [g (f x)] *)
let (@@) f x = f x
(* external (\@\@) : ('a -> 'b) -> 'a -> 'b = "%apply" *)

external id : 'a -> 'a = "%identity"
let flip f x y = f y x
let some x = Some x
let const x = fun () -> x

let apply2 f = fun (x,y) -> f x, f y

let printfn fmt = Printf.ksprintf print_endline fmt

let curry f a b = f (a, b)
let uncurry f (a,b) = f a b

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
let tuck l x = l := x :: !l
