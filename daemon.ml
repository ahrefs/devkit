
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
  begin match !pidfile with
  | Some pidfile -> Nix.manage_pidfile pidfile
(*   | None -> Exn.fail "PID file not specified" *)
  | None -> ()
  end;
  if not !foreground then Nix.daemonize ();
  Log.reopen !logfile;
  log #info "GC settings: %s" (Action.gc_settings ());
  Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> Log.reopen !logfile));
  Sys.set_signal Sys.sigusr2 (Sys.Signal_handle (fun _ -> Action.gc_show "compact" Gc.compact ()));

