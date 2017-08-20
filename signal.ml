(** Signal handling *)

open ExtLib
module U = ExtUnix.Specific
module Ev = Async.Ev

let log = Log.from "signal"

(** {2 libevent + signalfd} *)

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
  let (_:int list) = Unix.sigprocmask Unix.SIG_BLOCK sigs in
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

(** {2 Lwt} *)

let h_lwt = Hashtbl.create 10

let lwt_handle sigs f =
  sigs |> List.iter begin fun signo ->
    Option.may Lwt_unix.disable_signal_handler @@ Hashtbl.find_option h_lwt signo;
    let sig_id = Lwt_unix.on_signal signo (fun (_:int) -> f ()) in
    Hashtbl.replace h_lwt signo sig_id
  end

let lwt_handle_exit = lwt_handle [Sys.sigterm; Sys.sigint]
let lwt_handle_reload = lwt_handle [Sys.sighup]

(** {2 generic registration} *)

let install_sys signo f = Sys.set_signal signo (Sys.Signal_handle f)
let install_libevent t signo f = handle t [signo] f
let install_lwt signo f = lwt_handle [signo] (fun () -> f signo)

let h = Hashtbl.create 10
let verbose = ref false
let do_install = ref install_sys
let is_safe_output () = !verbose

let set sigs f =
  sigs |> List.iter begin fun signo ->
    let f =
      match Hashtbl.find_option h signo with
      | None -> f
      | Some g -> (fun n -> g n; f n)
    in
    Hashtbl.replace h signo f; !do_install signo f
  end

let set1 signal f = set [signal] (fun _ -> f ())

type state = (int, int -> unit) Hashtbl.t
let save () = Hashtbl.copy h
let restore x =
  Hashtbl.clear h;
  Hashtbl.iter (Hashtbl.add h) x

let replace sigs f =
  sigs |> List.iter (fun signo -> Hashtbl.replace h signo f; !do_install signo f)

let reinstall () = Hashtbl.iter !do_install h

let wrap name f =
  begin fun n ->
    if !verbose then log #info "Received signal %i (%s)..." n name;
    (try f () with exn -> if !verbose then log #warn ~exn "Signal handler failed");
    if !verbose then log #info "Signal handler done.";
  end

let set_exit f = set [Sys.sigterm; Sys.sigint] (wrap "exit" f)
let set_reload f = set [Sys.sighup] (wrap "reload" f)

let setup_sys () =
  verbose := false; (* potential deadlock *)
  do_install := install_sys;
  reinstall ()

let setup_libevent t =
  verbose := true;
  do_install := (install_libevent t);
  reinstall ()

let setup_lwt () =
  verbose := true;
  do_install := install_lwt;
  reinstall ()
