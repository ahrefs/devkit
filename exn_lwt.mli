(**
  Dealing with Lwt exceptions
*)

val catch : ('a -> 'b Lwt.t) -> 'a -> 'b option Lwt.t
val map : ('a -> 'b Lwt.t) -> 'a -> 'b Exn.result Lwt.t
val fail : ?exn:exn -> ('a, unit, string, 'b Lwt.t) format4 -> 'a
val invalid_arg : ('a, unit, string, 'b Lwt.t) format4 -> 'a

val exn_of_result : [`Ok of 'a | `Error of string] -> 'a Lwt.t
