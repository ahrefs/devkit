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

val set : int list -> (int -> unit) -> unit
val set_exit : (unit -> unit) -> unit
val set_reload : (unit -> unit) -> unit

val setup_sys : unit -> unit
val setup_libevent : t -> unit
val setup_lwt : unit -> unit
