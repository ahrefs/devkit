open Opentelemetry

module Traceparent = struct
  let name = Trace_context.Traceparent.name

  let get_ambient ?explicit_span () =
    match explicit_span, Trace_core.current_span () with
    | Some (Opentelemetry_trace.Extensions.Span_otel span), _
    | _, Some (Opentelemetry_trace.Extensions.Span_otel span) ->
        let ctx = Trace_context.Traceparent.to_value
          ~trace_id:(Span.trace_id span) ~parent_id:(Span.id span) () in
        Some ctx
    | _ -> None
end

let enter_manual_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name =
    let parent = Trace_core.current_span () in
    Trace_core.enter_span ~parent ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name
