
type json = [ `Float of float | `Int of int | `String of string ]

(** Export counters registered with {!Var} as logstash events *)
val get : unit -> [> `Assoc of (string * [> json ]) list ] list

(** Setup periodic saving of counters as logstash json rows along the logfile *)
val setup : ?pause:Time.t -> Libevent.event_base -> unit
val setup_lwt : ?pause:Time.t -> unit -> unit

type logger = <
  event : (string * Yojson.Safe.t) list -> unit; (** write event manually *)
  write : unit -> unit; (** write Var counters explicitly *)
  reload : unit -> unit; (** reopen output file *)
  flush : unit -> unit; (** force flush *)
>

(* Setup logger for a stream of events *)
val log : ?autoflush:float -> ?verbose:bool -> ?add_timestamp_only:bool -> ?name:string -> unit -> logger

val setup_error_log : unit -> unit

(** Counters with arbitrary attributes *)
module Dyn : sig
  type t = private (string * json) list
  val make : ?attrs:(string * json) list -> string -> t
  (* val add : t -> ?attrs:(string * string) list -> Var.t -> unit *)
  (* val set : t -> ?attrs:(string * string) list -> Var.t -> unit *)
  val set_count : t -> (string * json) list -> int -> unit
  val set_bytes : t -> (string * json) list -> int -> unit
  val set_time : t -> (string * json) list -> Time.t -> unit
  val add_count : t -> (string * json) list -> int -> unit
  val add_bytes : t -> (string * json) list -> int -> unit
  val add_time : t -> (string * json) list -> Time.t -> unit
end

(** Log events related to the life of the program:
    - [start]
    - [signal.stop]
    - [exit]
*)
val lifetime : ?extra:string -> ?start:Time.t -> events:logger -> version:string -> unit -> unit
