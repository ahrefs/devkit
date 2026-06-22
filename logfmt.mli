
(** Printer and parser for https://brandur.org/logfmt , good for structured logging. *)

val add_to_buffer : Buffer.t -> Logger.Pairs.t -> unit
val to_string : Logger.Pairs.t -> string

(** Parse logfmt lines *)
module Parser : sig
  (** Internal state *)
  type t

  val create : unit -> t

  (** Parse a whole logfmt line into its [key=value] pairs (in order). *)
  val parse : t -> string -> Logger.Pairs.t
end
