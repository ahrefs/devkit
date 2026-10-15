open Opentelemetry

let ambient_span ?explicit_span () =
  match explicit_span, Trace_core.current_span () with
  | Some (Opentelemetry_trace.Extensions.Span_otel span), _
  | _, Some (Opentelemetry_trace.Extensions.Span_otel span) -> Some span
  | _ -> None

module Trace_context = struct
  let header_names = Opentelemetry.Trace_context.[ Traceparent.name; Tracestate.name ]

  let get_ambient_headers ?explicit_span () =
    match ambient_span ?explicit_span () with
    | Some span -> Opentelemetry.Trace_context.headers_of_span_ctx (Span.to_span_ctx span)
    | None -> []
end

module Traceparent = struct
  let name = Opentelemetry.Trace_context.Traceparent.name

  let get_ambient ?explicit_span () =
    match ambient_span ?explicit_span () with
    | Some span ->
        Some (Opentelemetry.Trace_context.Traceparent.to_value
          ~trace_flags:(Span.trace_flags span) ~trace_id:(Span.trace_id span) ~parent_id:(Span.id span) ())
    | None -> None
end

(* no explicit [~parent]: the current span if any, otherwise let the collector look for
   an ambient context (e.g. from an incoming traceparent) instead of forcing a new trace *)
let enter_manual_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name =
    Trace_core.enter_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name
