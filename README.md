devkit
======

[![Build Status](https://github.com/ahrefs/devkit/actions/workflows/makefile.yml/badge.svg)](https://github.com/ahrefs/devkit/actions/workflows/makefile.yml)

General purpose OCaml library (development kit)
Copyright (c) 2009 Ahrefs
Released under the terms of LGPL-2.1 with OCaml linking exception.

Usage
-----

```sh
opam install devkit
```

### Tracing

Devkit's `Web` module includes both simple, configurable, local tracing support, as well as [distributed tracing][] support via OpenTelemetry.

To use local tracing, we use the mechanism provided by [`ocaml-trace`][]: by default, the tracing calls in devkit are a no-op; but if the top-level application containing devkit installs and configures a "tracing backend" at runtime, then devkit's web-requests will each produce timed traces.

> As an example, using `trace-tef` as a backend to produce a `json` file:
>
> ```ocaml
> open Devkit
>
> let run () =
>   let resp = Web.http_request `GET "http://icanhazip.com" in
>   (* ... *)
>
> let () =
>   Trace_tef.with_setup ~out:(`File "trace.json") () @@ fun () ->
>   run ()
> ```

For distributed traces, you'll both need to configure an OpenTelemetry backend for `ocaml-trace` at runtime (to collect devkit's own traces); and you will most likely also want to ensure your own application's traces (using either `ocaml-trace` or the full `ocaml-opentelemetry`) are properly configured to correctly appear as the _parent_ of devkit's traces.

Configuring an OpenTelemetry backend for the traces themselves is as simple wrapping your top-level application in a call to `Opentelemetry_trace.`[`setup_with_otel_backend`][] or the like. That will configure both OpenTelemetry's collector and the `ocaml-trace` backend.

Then, to ensure that parentage is correctly propagated across your distributed architecture, devkit can produce a W3C Trace Context [`traceparent`] HTTP header. To ensure that it produces the _correct_ `traceparent`, however, we depend upon 'ambient context'. For this to function properly, you'll need to follow the [detailed instructions in the `ocaml-ambient-context` documentation][ambient-context installation].

Once thus configured, devkit will check `Opentelemetry.Scope.`[`get_ambient_scope`][] before each HTTP request, and use the ambient tracing-span as the parent of the web-request's span; which will itself then be propagated to the remote service via the `traceparent` HTTP header.

> As an example, if your top-level application is using Lwt, and using cohttp as the OpenTelemetry backend:
>
> ```ocaml
> open Devkit
>
> let run () =
>   let* resp = Web.http_request_lwt `GET "http://icanhazip.com" in
>   (* ... *)
>
> let () =
>   Ambient_context.set_storage_provider (Ambient_context_lwt.storage ()) ;
>   Opentelemetry_trace.setup_with_otel_backend
>     (Opentelemetry_client_cohttp_lwt.create_backend ())
>   @@ fun () ->
>   Lwt_main.run @@ fun () ->
>   run ()
> ```

  [distributed tracing]: <https://opentelemetry.io/docs/concepts/signals/traces/> "OpenTelemetry: Traces"
  [`ocaml-trace`]: <https://github.com/c-cube/ocaml-trace> "Simon Cruanes' ocaml-trace library"
  [`setup_with_otel_backend`]: <https://v3.ocaml.org/p/opentelemetry/latest/doc/Opentelemetry_trace/index.html#val-setup_with_otel_backend> "ocaml-opentelemetry: Opentelemetry_trace.setup_with_otel_backend"
  [`traceparent`]: <https://www.w3.org/TR/trace-context/#traceparent-header> "W3C Trace Context specification: § 3.2 Traceparent header"
  [ambient-context installation]: <https://github.com/ELLIOTTCABLE/ocaml-ambient-context?tab=readme-ov-file#as-a-top-level-application> "ocaml-ambient-context: Installation (as a top-level application)"
  [`get_ambient_scope`]: <https://v3.ocaml.org/p/opentelemetry/latest/doc/Opentelemetry/Scope/index.html#val-get_ambient_scope>

Development
-----------

Install OCaml dependencies in your current / global switch:

    opam install . --deps-only

Or to install them in a new, directory-local switch:

    opam switch create . --deps-only --no-install
    opam install . --deps-only --with-test

External dependencies:

    opam list -s -e --resolve=devkit

To update ragel-generated code:

    aptitude install ragel
    make -B gen_ragel

To update metaocaml-generated code:

    opam exec --switch=4.07.1+BER -- make gen_metaocaml
