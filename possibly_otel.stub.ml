module Traceparent = struct
  let name = "traceparent"

  let get_ambient () = None
end


let enter_manual_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name =
  Trace_core.enter_manual_toplevel_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name
