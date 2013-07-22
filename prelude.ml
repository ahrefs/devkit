(** Useful shortcuts *)

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

(* src offset len *)
external md5_string: string -> int -> int -> string = "caml_md5_string"

(* oh ugly *)
let md5_hex_string s =
  let open ExtLib in
  let md5 = md5_string s 0 (String.length s) in
  let hex = function 0 -> '0' | 1 -> '1' | 2 -> '2' | 3 -> '3' | 4 -> '4' | 5 -> '5' | 6 -> '6' | 7 -> '7' | 8 -> '8' | 9 -> '9'
    | 10 -> 'a' | 11 -> 'b' | 12 -> 'c' | 13 -> 'd' | 14 -> 'e' | 15 -> 'f' | _ -> raise (Invalid_argument s)
  in
  String.init 32 (fun i ->
    let op = if i mod 2 = 0 then (/) else (mod) in
    hex (op (int_of_char (md5.[i / 2])) 16)
  )
