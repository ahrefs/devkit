module Otrace := Trace_core

module Trace_context : sig
  val header_names : string list
  (** Names of the W3C trace context headers ([traceparent], [tracestate]) *)

  val get_ambient_headers : ?explicit_span:Trace_core.span -> unit -> (string * string) list
  (** Headers propagating the context of [explicit_span], or else of the ambient span:
      [traceparent] with the span's sampled flag, and [tracestate] if non-empty.
      Empty if there is no OTEL span. *)
end

module Traceparent : sig
  val name : string
  val get_ambient : ?explicit_span:Trace_core.span -> unit -> string option
end [@@deprecated "use Trace_context, which also propagates tracestate"]

val enter_manual_span :
  __FUNCTION__:string ->
  __FILE__:string ->
  __LINE__:int ->
  ?data:(unit -> (string * Otrace.user_data) list) ->
  string ->
  Trace_core.span
