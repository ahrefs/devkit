(** Various utilities for use with Lwt. *)

val with_count : int ref -> 'a Lwt.t -> 'a Lwt.t

val timely : Time.t -> ('a -> unit Lwt.t) -> ('a -> unit Lwt.t)

(** [timely_loop' ?immediate period f] run f every period seconds; run immediately if immediate is true. *)
val timely_loop' : ?immediate:bool -> Time.t -> (unit -> unit Lwt.t) -> unit Lwt.t

(** [timely_loop' ?immediate ?wait period f] run f every period seconds; run immediately if immediate is true; stop when wait thread terminates. *)
val timely_loop : ?immediate:bool -> ?wait:unit Lwt.t -> Time.t -> (unit -> unit Lwt.t) -> unit Lwt.t

(** [ensure_order t1 t2] cancel t1 when t2 terminates. *)
val ensure_order : 'a Lwt.t -> 'b Lwt.t -> 'b Lwt.t

(** [suppress_exn name cleanup t] wait for t to terminate, suppress any exception, and call cleanup () afterwards. *)
val suppress_exn : string -> (unit -> 'a Lwt.t) -> unit Lwt.t -> 'a Lwt.t

val action : string -> ('a -> 'b Lwt.t) -> 'a -> 'b Lwt.t

val action_do : string -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(** same as [Lwt.async] but also cancels task on {!Daemon.ShouldExit} *)
val async : (unit -> unit Lwt.t) -> unit

(** [idle_check ~interval] is a pair [(stamp,wait)] where you use
    [stamp: unit -> unit] to indicate activity, and [wait : unit Lwt.t] is a
    promise that resolves if there's been no calls to [stamp] during an
    [interval].

    This is typically used to manage a background task (e.g., periodically
    fetching data from a remote source) based on whether there is ongoing
    activity (e.g., whether the data is being used in the UI). *)
val idle_check : interval:Time.duration -> ((unit -> unit) * unit Lwt.t)
