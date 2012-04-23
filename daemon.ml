
module U = ExtUnix.Specific

let log = Log.from "daemon"

let logfile = ref None
let pidfile = ref None
let runas = ref None
let foreground = ref false

let args =
  [
    ExtArg.may_str "logfile" logfile "<file> Log file";
    ExtArg.may_str "pidfile" pidfile "<file> PID file";
    "-runas",
      Arg.String (fun name -> try runas := Some (Unix.getpwnam name) with exn -> Exn.fail "runas: unknown user %s" name),
      "<user> run as specified user"; 
    "-fg", Arg.Set foreground, " Stay in foreground";
  ]

let manage () =
  if not !foreground then Nix.daemonize ();
  begin match !runas with
  | None -> ()
  | Some pw ->
    let uid = pw.Unix.pw_uid and gid = pw.Unix.pw_gid in
    U.setreuid uid uid;
    U.setregid gid gid;
  end;
  Log.reopen !logfile; (* immediately after fork *)
  Option.may Nix.manage_pidfile !pidfile; (* after fork! *)
  log #info "GC settings: %s" (Action.gc_settings ());
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> Log.reopen !logfile));
  Sys.set_signal Sys.sigusr2 (Sys.Signal_handle (fun _ -> U.malloc_stats (); Action.gc_show "compact" Gc.compact ()));

