(**
  Signal handling via signalfd
*)

open ExtLib
module U = ExtUnix.Specific
module Ev = Async.Ev

let log = Log.from "signal"

type t = { ev : Ev.event; fd : Unix.file_descr; h : (int, (int -> unit)) Hashtbl.t; mutable active : bool; }

let init events =
  let fd = U.signalfd ~sigs:[] ~flags:[] () in
  Unix.set_nonblock fd;
  let t = { ev = Ev.create (); fd = fd; h = Hashtbl.create 1; active = true; } in
  Ev.set events t.ev t.fd ~persist:true [Ev.READ] (fun _ _ ->
    try (* references to t keep it alive with ev *)
      let ssi = U.signalfd_read t.fd in
      let signo = U.ssi_signo_sys ssi in
      match Hashtbl.find_option t.h signo with
      | None -> Exn.fail "no handler for %d" signo
      | Some f -> f signo
    with exn -> log #warn ~exn "signal handler"
  );
  Ev.add t.ev None;
  t

let stop t =
  match t.active with
  | false -> ()
  | true ->
    Ev.del t.ev;
    Hashtbl.clear t.h;
    Unix.close t.fd;
    t.active <- false

let handle t sigs f =
  List.iter (fun signo -> Hashtbl.replace t.h signo f) sigs;
  let sigs = List.of_enum (Hashtbl.keys t.h) in
  let (_:int list) = Thread.sigmask Unix.SIG_BLOCK sigs in
  let _ = U.signalfd ~fd:t.fd ~sigs ~flags:[] () in
  ()

let handle_exit t f = handle t [Sys.sigterm; Sys.sigint]
  (fun n ->
    log #info "Received signal %i (exit)..." n;
    (try f () with exn -> log #warn ~exn "Signal.handle_exit");
    log #info "Signal handler done.";)

let handle_reload t f = handle t [Sys.sighup]
  (fun n ->
    log #info "Received signal %i (reload)..." n;
    (try f () with exn -> log #warn ~exn "Signal.handle_reload");
    log #info "Signal handler done.")
