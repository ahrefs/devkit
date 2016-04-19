(** Signal handling *)

(** {2 libevent + signalfd}

  explicit interface (for compatibility)
*)

type t
val init : Async.Ev.event_base -> t
val stop : t -> unit
val handle : t -> int list -> (int -> unit) -> unit
val handle_exit : t -> (unit -> unit) -> unit
val handle_reload : t -> (unit -> unit) -> unit

(** {2 lwt}

  explicit interface (for compatibility)
*)

val lwt_handle : int list -> (unit -> unit) -> unit
val lwt_handle_exit : (unit -> unit) -> unit
val lwt_handle_reload : (unit -> unit) -> unit

(** {2 generic registration} *)

val is_safe_output : unit -> bool

(** add signal handler for specified signals *)
val set : int list -> (int -> unit) -> unit
val set1 : int -> (unit -> unit) -> unit
val set_exit : (unit -> unit) -> unit
val set_reload : (unit -> unit) -> unit

(** replace signal handler for specified signals *)
val replace : int list -> (int -> unit) -> unit

(** setup "standard" signal driver, deadlock-friendly, default *)
val setup_sys : unit -> unit

(** setup signals via libevent (signalfd), requires event loop *)
val setup_libevent : t -> unit

(** setup signals via lwt, requires {!Lwt_main.run} *)
val setup_lwt : unit -> unit
