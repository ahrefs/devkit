(** Bare-bones httpev server example *)

open Printf
open Devkit

let log = Httpev.Hidden.log

let http_handle _st req k_http =
  let module Arg = Httpev.Args(struct let req = req end) in
  match req.Httpev.path with
  | "/hello" ->
    let name = Option.default "world" (Arg.get "name") in
    k_http @@ Httpev.serve_text req (sprintf "Hello, %s!" name)
  | _ ->
    log #warn "not found : %s" (Httpev.show_request req);
    k_http @@ Httpev.not_found

let run http_port =
  let main () =
    let http_config = { Httpev.default with
      Httpev.events = Async.Ev.init ();
      connection = ADDR_INET (Unix.inet_addr_any, http_port);
      max_request_size = 128 * 1024;
    } in
    Httpev.server http_config http_handle;
  in
  Action.log main ()
