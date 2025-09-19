(** Useful shortcuts *)

module U = ExtUnix.Specific
module Enum = ExtEnum

let ($) f g = fun x -> f (g x)
let ($$) f g = fun x y -> f (g x) (g y)
let (!!) = Lazy.force

module F1 = struct
  let (@@) f g = fun x -> f @@ g @@ x
  let (|>) f g = fun x -> x |> f |> g
end

external id : 'a -> 'a = "%identity"
external identity : 'a -> 'a = "%identity"
let flip f x y = f y x
let some x = Some x
let const x = fun () -> x

let apply2 f = fun (x,y) -> f x, f y

let printfn fmt = Printf.ksprintf print_endline fmt
let eprintfn fmt = Printf.ksprintf prerr_endline fmt

let curry f a b = f (a, b)
let uncurry f (a,b) = f a b

module Fresh(T : sig type t val compare : t -> t -> int end)() =
struct
  type t = T.t
  let inject = id
  let project = id
  let inject_list = id
  let project_list = id
  let compare = T.compare
  let equal a b = T.compare a b = 0
  let map f x = inject @@ f @@ project x
  let map2 f x y = inject @@ f (project x) (project y)
end

let (+=) a b = a := !a + b
let (-=) a b = a := !a - b
let tuck l x = l := x :: !l
let cons l x = x :: l

let round f =
  let bot = floor f in
  if f -. bot < 0.5 then bot else bot +. 1.

let atoi name v = try int_of_string v with _ -> Exn.fail "%s %S not integer" name v

let call_me_maybe f x =
  match f with
  | None -> ()
  | Some f -> f x

let () =
  Lwt_engine.set @@ new Lwt_engines.poll
