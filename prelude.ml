(** Useful shortcuts *)

open ExtLib

module U = ExtUnix.Specific
module Enum = ExtEnum

let ($) f g = fun x -> f (g x)
let ($$) f g = fun x y -> f (g x) (g y)
let (!!) = Lazy.force

IFNDEF OCAML401 THEN
(** reverse apply : [x |> f |> g] is equivalent to [g (f x)] *)
IFDEF OCAML400 THEN
external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"
ELSE
let (|>) x f = f x
ENDIF
ENDIF

IFNDEF OCAML401 THEN
(** apply : [g \@\@ f \@\@ x] is equivalent to [g (f x)] *)
IFDEF OCAML400 THEN
external (@@) : ('a -> 'b) -> 'a -> 'b = "%apply"
ELSE
let (@@) f x = f x
ENDIF
ENDIF

(** @deprecated in favor of [|>] *)
let (>>) = (|>)
(** @deprecated in favor of [\@\@] *)
let (&) = (@@)

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

module New(T : sig type t end) :
sig
  type t
  val inj : T.t -> t
  val proj : t -> T.t
  val inj_list : T.t list -> t list
  val proj_list : t list -> T.t list
end =
struct
  type t = T.t
  let inj = id
  let proj = id
  let inj_list = id
  let proj_list = id
end

let cons x y = x :: y
let (+=) a b = a := !a + b
let (-=) a b = a := !a - b
let tuck l x = l := x :: !l

let round f =
  let bot = floor f in
  if f -. bot < 0.5 then bot else bot +. 1.

let atoi name v = try int_of_string v with _ -> Exn.fail "%s %S not integer" name v

let call_me_maybe f x =
  match f with
  | None -> ()
  | Some f -> f x

class poll_engine =
let readmask = U.Poll.(pollin + pollerr + pollhup + pollpri + pollrdhup) in
let writemask = U.Poll.(pollout + pollerr + pollhup) in
object
inherit Lwt_engine.poll_based
method poll fds timeout =
(*
  let show = Action.strl (fun (fd,i,o) -> sprintf "%d%s%s" (U.int_of_file_descr fd) (if i then "r" else "") (if o then "w" else "")) in
  log #info "lwt poll %f %s" timeout (show fds);
*)
  let fds = List.map (fun (fd,i,o) -> fd, U.Poll.((if i then pollin else none) + (if o then pollout else none))) fds in
  let l = U.poll (Array.of_list fds) timeout |> List.map (fun (fd,f) -> fd, U.Poll.is_inter f readmask, U.Poll.is_inter f writemask) in
(*   log #info "lwt poll done %s" (show l); *)
  l
end

let () =
  Lwt_engine.set (new poll_engine)
