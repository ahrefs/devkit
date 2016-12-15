(** daemon utilities *)

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
let should_exit_ = ref false
let (should_exit_lwt,signal_exit_lwt) = Lwt.wait ()
let should_exit () = !should_exit_
let should_run () = not !should_exit_

(** exception to be raised by functions that wish to signal premature termination due to [!should_exit = true] *)
exception ShouldExit

let signal_exit =
  let do_lwt = lazy (Lwt.wakeup_later signal_exit_lwt ()) in
  (* invariant: should_exit_ = (Lwt.state should_exit_lwt = Lwt.Return) *)
  fun () -> should_exit_ := true; Lazy.force do_lwt

(** raise [ShouldExit] if [should_exit] condition is set, otherwise do nothing *)
let break () = if !should_exit_ then raise ShouldExit

(** wait until [should_exit] is set and raise [ShouldExit] *)
let wait_exit () = Lwt.bind should_exit_lwt (fun () -> Lwt.fail ShouldExit)

(** obsolete, use [wait_exit] *)
let break_lwt = wait_exit

let get_args () =
  [
    (let set_loglevel s =
       Stre.nsplitc s ',' |> List.iter begin fun spec ->
         match Stre.nsplitc spec '=' with
         | facil :: l :: [] -> Log.set_filter ~name:facil (Logger.level l)
         | l :: [] -> Log.set_filter @@ Logger.level l
         | _ -> Exn.fail "loglevel not recognized, specify either <level> or <facil>=<level>"
       end
     in
     "-loglevel", Arg.String set_loglevel, " ([<facil>=]debug|info|warn|error[,])+");
    ExtArg.may_str "logfile" logfile "<file> Log file";
    ExtArg.may_str "pidfile" pidfile "<file> PID file";
    ExtArg.may_str "logrotation" logrotation "t<time_hours>|s<size_MB|onceaday> log rotation options";
    "-runas",
      Arg.String (fun name -> try runas := Some (Unix.getpwnam name) with exn -> Exn.fail ~exn "runas: unknown user %s" name),
      "<user> run as specified user";
    "-fg", Arg.Set foreground, " Stay in foreground";
  ]

let args = get_args ()

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
  Log.read_env_config ();
  Option.may Nix.manage_pidfile !pidfile; (* write pidfile after fork! *)
  if Option.is_some !logfile then
  begin
    let quote s = try Scanf.sscanf s "%_[a-zA-Z0-9:_/.-]%!" s with _ -> Filename.quote s in
    log #info "run: %s" (String.concat " " (List.map quote (Array.to_list Sys.argv)));
    log #info "GC settings: %s" (Action.gc_settings ());
  end;
  let unix_stderr s =
    let s = Log.State.format_simple `Info log#facility s in
    try
      let (_:int) = Unix.write Unix.stderr s 0 (String.length s) in ()
    with _ ->
      () (* do not fail, can be ENOSPC *)
  in
  Signal.set [Sys.sigpipe] ignore;
  Signal.set [Sys.sigusr1] (fun _ -> Log.reopen !logfile);
  Signal.set [Sys.sigusr2] begin fun _ ->
    match Signal.is_safe_output () with
    | true -> Memory.log_stats (); Memory.reclaim ()
    | false ->
      (* output directly to fd to prevent deadlock, but breaks buffering *)
      Memory.get_stats () |> List.iter unix_stderr;
      Memory.reclaim_s () |> unix_stderr
  end;
  Signal.set_exit signal_exit;
  Nix.raise_limits ();
  managed := true;
  ()
