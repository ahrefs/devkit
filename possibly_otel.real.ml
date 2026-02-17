open Opentelemetry

let[@inline] (let*) o f = Option.map f o

open  Opentelemetry_trace.Extensions

module Traceparent = struct
  let name = Trace_context.Traceparent.name

  let get_ambient ?explicit_span () =
    match explicit_span with
    | Some (Span_otel sp) ->
      let scope = scope_of_span_info sp in
      let tp =
        Trace_context.Traceparent.to_value ~trace_id:scope.trace_id
        ~parent_id:scope.span_id () in
      Some tp
    | _ ->
      let* sp = Scope.get_ambient_scope () in
      Trace_context.Traceparent.to_value
        ~trace_id:sp.trace_id ~parent_id: sp.span_id ()
end

let enter_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name =
  (* opentelemetry.trace will retrieve parent scope just fine *)
  Trace_core.enter_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name
