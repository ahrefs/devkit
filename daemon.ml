
let log = Log.from "daemon"

let logfile = ref None
let pidfile = ref None
let foreground = ref false

let args =
  [
    ExtArg.may_str "logfile" logfile "<file> Log file";
    ExtArg.may_str "pidfile" pidfile "<file> PID file";
    "-fg", Arg.Set foreground, " Stay in foreground";
  ]

let manage () =
  if not !foreground then Nix.daemonize ();
  Log.reopen !logfile; (* immediately after fork *)
  Option.may Nix.manage_pidfile !pidfile; (* after fork! *)
  log #info "GC settings: %s" (Action.gc_settings ());
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> Log.reopen !logfile));
  Sys.set_signal Sys.sigusr2 (Sys.Signal_handle (fun _ -> Nix.malloc_stats (); Action.gc_show "compact" Gc.compact ()));

