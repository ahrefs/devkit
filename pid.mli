(** Unique process identification *)

type t = {
  host : string; (** machine hostname (no spaces allowed) *)
  id : int; (** process id *)
  name : string; (** application id (no spaces allowed), for information. *)
  stamp : int; (** stamp for uniqueness to guard against pid reuse *)
}

(** dummy instance, use sparingly *)
val dummy : t

(** @return pretty-printed pid (human readable) *)
val show : t -> string

(** @return machine hostname *)
val host : t -> string

(** @return application name *)
val name : t -> string

(** @return string representation of pid, can be read back by [parse_pid_exn] *)
val to_string : t -> string

val make : id:int -> host:string -> stamp:int -> string -> t

val compare : t -> t -> int
val equal : t -> t -> bool

val parse_exn : string -> t

(** {1 Current process identifier} *)

val set_name : string -> unit
val self : unit -> t
val self_name : unit -> string
val self_as : string -> t
val show_self : unit -> string

(** call this to update Pid.self after fork *)
val update : unit -> unit

(**/**)

val set_fake : t -> unit
