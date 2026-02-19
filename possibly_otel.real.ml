open Opentelemetry

let[@inline] (let*) o f = Option.map f o

open  Opentelemetry_trace.Extensions

module Traceparent = struct
  let name = Trace_context.Traceparent.name

  let get_ambient ?explicit_span () =
    match explicit_span with
    | Some (Span_otel sp) ->
      let tp =
        Trace_context.Traceparent.to_value ~trace_id:(Trace_id.of_bytes sp.trace_id)
        ~parent_id:(Span_id.of_bytes sp.span_id) () in
      Some tp
    | _ ->
      let* sp = Ambient_span.get () in
      Trace_context.Traceparent.to_value
        ~trace_id:(Trace_id.of_bytes sp.trace_id)
        ~parent_id:(Span_id.of_bytes sp.span_id) ()
end

let enter_manual_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name =
  let parent = Ambient_span.get () |> Option.map (fun sp -> Span_otel sp) in
  Trace_core.enter_span ~parent ~__FUNCTION__ ~__FILE__ ~__LINE__ ?data name
