open Opentelemetry

let (let*) o f = Option.map f o

module Traceparent = struct
  let name = Trace_context.Traceparent.name

  let get_ambient ?explicit_span:_ () =
    let* Scope.{ trace_id; span_id; _ } = Scope.get_ambient_scope () in
    Trace_context.Traceparent.to_value ~trace_id ~parent_id:span_id ()
end

let enter_manual_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name =
      Trace_core.enter_span ~parent:None ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name
