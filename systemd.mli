(**
   Misc utils for systemd.
*)

(** Subset of [sd-daemon] in ocaml.

    The following functionalities are provided:
    - File descriptor passing for socket-based activation
    - Detection of systemd boots
*)
module Daemon : sig

  (** [true] if the system was booted with systemd. *)
  val booted : bool

  (**
     Returns file descriptors that have been passed by systemd.

     This function call ensures that the [FD_CLOEXEC] flag is set for
     the passed file descriptors, to make sure they are not passed on
     to child processes. If [FD_CLOEXEC] shall not be set, the caller
     needs to unset it after this call for all file descriptors that
     are used.
  *)
  val listen_fds : unit -> Unix.file_descr list

  (** Same as {!listen_fds} but return lwt file descriptors. *)
  val listen_fds_lwt : unit -> Lwt_unix.file_descr list

  (** Similar to {!Daemon.get_args} but without the foregound and pidfile
      option. *)
  val get_args : unit -> (string * Arg.spec * string) list

  (** Similar to {!Daemon.manage} but sets to run in the foreground. *)
  val manage : unit -> unit
end
