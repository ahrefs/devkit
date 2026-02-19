open Opentelemetry

let (let*) o f = Option.map f o

module Traceparent = struct
  let name = Trace_context.Traceparent.name

  let get_ambient ?explicit_span () =
    let* Scope.{ trace_id; span_id; _ } = Scope.get_ambient_scope () in
    let span_id = match explicit_span with
      | Some {Trace_core.span; _} -> Opentelemetry_trace.Conv.span_id_to_otel span
      | None -> span_id 
    in
    Trace_context.Traceparent.to_value ~trace_id ~parent_id:span_id ()
end

let enter_manual_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name =
    match Scope.get_ambient_scope () with
    | None ->
      Trace_core.enter_manual_span ~parent:None ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name
    | Some Scope.{ span_id; trace_id; _ } ->
      let otrace_espan = Trace_core.{
        span = Opentelemetry_trace.Conv.span_id_of_otel span_id;
        trace_id = Opentelemetry_trace.Conv.trace_id_of_otel trace_id;
        meta = Trace_core.Meta_map.empty
      } in
      let parent = Some (Trace_core.ctx_of_span otrace_espan) in
      Trace_core.enter_manual_span ~parent ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name
