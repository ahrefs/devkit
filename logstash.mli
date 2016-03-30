
(** Export counters registered with {!Var} as logstash events *)
val get : unit -> [> `Assoc of (string * [>`Floatlit of string | `Int of int | `String of string ]) list ] list

(** Setup periodic saving of counters as logstash json rows along the logfile *)
val setup : ?pause:Time.t -> Libevent.event_base -> unit
val setup_lwt : ?pause:Time.t -> unit -> unit

(* Setup logger for a stream of events *)
val log : unit -> < event : (string * Yojson.json) list -> unit; write : unit -> unit; reload : unit -> unit >

(** Counters with arbitrary attributes *)
module Dyn : sig
  type t = private (string * string) list
  val make : ?attrs:(string * string) list -> string -> t
  (* val add : t -> ?attrs:(string * string) list -> Var.t -> unit *)
  (* val set : t -> ?attrs:(string * string) list -> Var.t -> unit *)
  val set_count : t -> ?attrs:(string * string) list -> int -> unit
  val set_bytes : t -> ?attrs:(string * string) list -> int -> unit
  val set_time : t -> ?attrs:(string * string) list -> Time.t -> unit
  val add_count : t -> ?attrs:(string * string) list -> int -> unit
  val add_bytes : t -> ?attrs:(string * string) list -> int -> unit
  val add_time : t -> ?attrs:(string * string) list -> Time.t -> unit
end
