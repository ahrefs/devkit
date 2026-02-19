module Traceparent = struct
  let name = "traceparent"

  let get_ambient ?explicit_span:_ () = None
end


let enter_manual_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name =
  Trace_core.enter_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name
