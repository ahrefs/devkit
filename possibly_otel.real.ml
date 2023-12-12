module Otel = Opentelemetry

let get_traceparent () =
    let tracing_scope = Otel.Scope.get_ambient_scope () in

    match tracing_scope with
    | None -> None
    | Some Otel.Scope.{ trace_id; span_id; _ } ->
      let tp_value = Otel.Trace_context.Traceparent.to_value ~trace_id ~parent_id:span_id () in
      Some (Otel.Trace_context.Traceparent.name ^ ": " ^ tp_value)

let enter_manual_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name =
    let tracing_scope = Otel.Scope.get_ambient_scope () in

    match tracing_scope with
    | None ->
      Trace_core.enter_manual_toplevel_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name
    | Some Otel.Scope.{ span_id; _ } ->
      let otrace_espan = Trace_core.{
        span = Opentelemetry_trace.Internal.otrace_of_otel span_id;
        meta = Trace_core.Meta_map.empty
      } in
      Trace_core.enter_manual_sub_span ~parent:otrace_espan ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name
