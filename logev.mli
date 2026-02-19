type attribute_value =
  [ `Int of int
  | `String of string
  | `Bool of bool
  | `Float of float
  | `None
  ]
type attribute = string * attribute_value

(** A rich log event/wide event/span.

  Will emit a log upon exit, but also possibly a span. It is encouraged to
  add many attributes to it, they might be useful later during debugging.

  https://jeremymorrell.dev/blog/a-practitioners-guide-to-wide-events/ for some context. *)
class type t = object
  (** Add an attribute to the event. For example,
      [logev # set_attr "duration" (`Float 42.5)]. *)
  method set_attr : string -> attribute_value -> unit

  method set_attrs : attribute list -> unit

  (** Record  that an exception happened. This also marks the log event
      as failed. *)
  method record_exn : exn -> Printexc.raw_backtrace -> unit

  (** This event is finished, emit the log record, etc. *)
  method finish : unit

  (** Emit a log message inside the bigger log event, with the same level. *)
  method log : 'a. ('a, unit, string, unit) format4 -> 'a

  method span : Trace_core.span
end

val dummy : t

val set_attr : #t -> string -> attribute_value -> unit
val set_attrs : #t -> attribute list -> unit

type 'a enter_args =
  ?__FUNCTION__:string ->
  __FILE__:string ->
  __LINE__:int ->
  ?attrs:attribute list ->
  log:Log.logger ->
  'a

(** Enter a Log event.
    @param level the level used for logev enter/exit logs, and for the Trace span (default `Debug) *)
val enter : (?level:Logger.level -> string -> t) enter_args

val enter_info : (string -> t) enter_args
val enter_error : (string -> t) enter_args
val enter_debug : (string -> t) enter_args
val enter_warn : (string -> t) enter_args

(** Exit. *)
val exit : #t -> unit

(** Enricher: add attributes to every single log events. *)
module Enricher : sig
  type t = unit -> attribute list

  (** [add f] adds [f] to a list of enrichers. When we create a logev,
    [f()] is called to produce attributes that are added to the logev.
    It must therefore be cheap.

    Some enrichers are already registered by default, including GC/VM statistics,
    daemon information, and basic info about the process. *)
  val add : t -> unit

  (** Cache values, refresh if they're older than [timeout] seconds.
     This is useful to enrich every log event with information that's costly
     to obtain, e.g. full GC statistics. Instead we only recompute
     every [timeout_s] and use the cached information, so the cost is amortized. *)
  val cached : timeout_s:float -> t -> t
end
