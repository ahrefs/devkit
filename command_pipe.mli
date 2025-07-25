(** Command pipe  var/control.PID.fifo that reads commands one per line
    and runs the corresponding function. The FIFO is removed automatically
    on process exit *)

type t

(** Create FIFO and start processing commands. *)
val make : unit -> t

(** Stop processing commands and close file. *)
val kill : t -> unit Lwt.t

(** Define new command for the control FIFO. *)
val add_command : t -> string -> (unit -> unit Lwt.t) -> unit
