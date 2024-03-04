module Otrace := Trace_core

module Traceparent : sig
  val name : string
  val get_ambient : unit -> string option
end

val enter_manual_span :
  __FUNCTION__:string ->
  __FILE__:string ->
  __LINE__:int ->
  ?data:(unit -> (string * Otrace.user_data) list) ->
  string ->
  Trace_core.explicit_span

val add_data_to_manual_span :
  Trace_core.explicit_span -> (string * Otrace.user_data) list -> unit
