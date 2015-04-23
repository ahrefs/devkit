
type t = private { host : string; id : int; name : string; stamp : int; }

val dummy : t

val show : t -> string
val string_of_pid : t -> string
val short_name : t -> string

val compare : t -> t -> int
val equal : t -> t -> bool

val parse_pid_exn : string -> t

val self_as : string -> t
val set_name : string -> unit
val self : unit -> t
val show_self : unit -> string
