open Opentelemetry

let[@inline]  (let*) o f = Option.map f o

type Trace_core.span +=
  | Span_otel of Scope.t

module Traceparent = struct
  let name = Trace_context.Traceparent.name

  let get_ambient ?explicit_span () =
    let* Scope.{ trace_id; span_id; _ } = Scope.get_ambient_scope () in
    let span_id = match explicit_span with
      | Some {Trace_core.span; _} -> Opentelemetry_trace.Internal.otel_of_otrace span
      | None -> span_id 
    in
    Trace_context.Traceparent.to_value ~trace_id ~parent_id:span_id ()
end

let enter_manual_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name =
    match Scope.get_ambient_scope () with
    | None ->
      Trace_core.enter_manual_toplevel_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name
    | Some Scope.{ span_id; _ } ->
      let otrace_espan = Trace_core.{
        span = Opentelemetry_trace.Internal.otrace_of_otel span_id;
        meta = Trace_core.Meta_map.empty
      } in
      Trace_core.enter_manual_sub_span ~parent:otrace_espan ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name
