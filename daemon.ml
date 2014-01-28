(** daemon utilities *)

open Prelude

module U = ExtUnix.Specific

let log = Log.from "daemon"

let logfile = ref None
let pidfile = ref None
let runas = ref None
let foreground = ref false
let logrotation = ref None

let managed = ref false

(** global flag indicating that process should exit,
    [manage] will automatically set this flag on SIGTERM unless default signal handling is overriden
*)
let should_exit = ref false

let args =
  [
    ExtArg.may_str "logfile" logfile "<file> Log file";
    ExtArg.may_str "pidfile" pidfile "<file> PID file";
    ExtArg.may_str "logrotation" logrotation "t<time_hours>|s<size_MB|onceaday> log rotation options";
    "-runas",
      Arg.String (fun name -> try runas := Some (Unix.getpwnam name) with exn -> Exn.fail ~exn "runas: unknown user %s" name),
      "<user> run as specified user"; 
    "-fg", Arg.Set foreground, " Stay in foreground";
  ]

let manage () =
  match !managed with
  | true -> () (* be smart *)
  | false ->
  (* check logrotation param before daemonize *)
  let lrot = ref Log.No_rotation in
  begin match !logfile, !logrotation with
  | None, Some _ -> Exn.fail "log rotation cannot be used without specified logfile"
  | _, None -> ()
  | _, Some s when String.length s = 0 -> Exn.fail "No rotation format specified, use manual for more info"
  | Some _, Some s when s.[0] = 't' -> lrot := Log.Days_rotation (int_of_string (String.sub s 1 (String.length s - 1)))
  | Some _, Some s when s.[0] = 's' -> lrot := Log.Size_rotation (int_of_string (String.sub s 1 (String.length s - 1)))
  | Some _, Some "onceaday" -> lrot := Log.OnceAday_rotation
  | _, Some s -> Exn.fail "bad log rotation format %s, use d<days> or s<size MB>" s
  end;
(*
  this will fail if files don't exists :(
  (* fail before fork if something is wrong *)
  Option.may (fun path -> Unix.(access path [R_OK;W_OK])) !logfile;
  Option.may (fun path -> Unix.(access path [R_OK;W_OK])) !pidfile;
*)
  Option.may Nix.check_pidfile !pidfile; (* check pidfile before fork to fail early *)
  if not !foreground then Nix.daemonize ();
  begin match !runas with
  | None -> ()
  | Some pw ->
    let uid = pw.Unix.pw_uid and gid = pw.Unix.pw_gid in
    U.setreuid uid uid;
    U.setregid gid gid;
  end;
  Log.reopen !logfile; (* immediately after fork *)
  Log.set_rotation !lrot;
  Option.may Nix.manage_pidfile !pidfile; (* write pidfile after fork! *)
  if Option.is_some !logfile then
  begin
    let quote s = try Scanf.sscanf s "%_[a-zA-Z0-9:_/.-]%!" s with _ -> Filename.quote s in
    log #info "run: %s" (String.concat " " @@ List.map quote @@ Array.to_list Sys.argv);
    log #info "GC settings: %s" (Action.gc_settings ());
  end;
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> Log.reopen !logfile));
  Sys.set_signal Sys.sigusr2 (Sys.Signal_handle (fun _ -> U.malloc_stats (); Action.gc_show "compact" Gc.compact ()));
  Nix.handle_sig_exit_with ~exit:false (fun () -> should_exit := true);
  managed := true;
  ()
