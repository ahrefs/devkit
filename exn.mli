(**
  Dealing with exceptions
*)

val catch : ('a -> 'b) -> 'a -> 'b option
val default : 'b -> ('a -> 'b) -> 'a -> 'b
val suppress : ('a -> unit) -> 'a -> unit

type 'a result = [ `Ok of 'a | `Exn of exn ]

val map : ('a -> 'b) -> 'a -> 'b result

(** Like [Printexc.to_string] but add more info when possible. *)
val to_string : exn -> string

(** alias to [to_string]. *)
val str : exn -> string

(** Like [failwith], [invalid_arg] but with format string. *)

val fail : ?exn:exn -> ('a, unit, string, 'b) format4 -> 'a
val invalid_arg : ('a, unit, string, 'b) format4 -> 'a

(** Like [Printexc.get_backtrace] but splits result on '\n'. *)
val get_backtrace : unit -> string list
