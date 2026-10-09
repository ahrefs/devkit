module Trace_context = struct
  let header_names = [ "traceparent"; "tracestate" ]

  let get_ambient_headers ?explicit_span:_ () = []
end

module Traceparent = struct
  let name = "traceparent"

  let get_ambient ?explicit_span:_ () = None
end


let[@inline] enter_manual_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name =
  Trace_core.enter_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name
