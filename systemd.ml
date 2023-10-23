open ExtLib

let log = Log.from "systemd"

module Daemon = struct

  (* https://github.com/systemd/systemd/blob/cb3108669d623afe58a36077e36ae4c66ff7d1c3/src/systemd/sd-daemon.h#L56 *)
  (* The first passed file descriptor is fd 3. *)
  let sd_listen_fds_start = 3

  let booted =
    (* https://github.com/systemd/systemd/blob/cb31086/src/libsystemd/sd-daemon/sd-daemon.c#L607 *)
    try
      Unix.access "/run/systemd/system/" Unix.[F_OK];
      true
    with Unix.Unix_error _ ->
      false

  let listen_pid =
    Option.map int_of_string (Sys.getenv_opt "LISTEN_PID")

  let listen_fds () : Unix.file_descr list =
    (* https://github.com/systemd/systemd/blob/cb31086/src/libsystemd/sd-daemon/sd-daemon.c#L42-L90 *)
    match booted with
    | false ->
      log#debug "listen_fds: not booted with systemd";
      []
    | true ->
    match listen_pid with
    | None ->
      log#debug "listen_fds: no LISTEN_PID";
      []
    | Some listen_pid ->
    let self_pid = Unix.getpid () in
    match listen_pid = self_pid with
    | false ->
      log#warn "listen_fds: LISTEN_PID %d and process pid %d are not equal, ignoring" listen_pid self_pid;
      []
    | true ->
    let listen_fds = Option.map int_of_string (Sys.getenv_opt "LISTEN_FDS") in
    match listen_fds with
    | None ->
      log#warn "listen_fds: LISTEN_PID, but no LISTEN_FDS";
      []
    | Some n when n <= 0 ->
      log#warn "listen_fds: LISTEN_FDS %d is not positive" n;
      []
    | Some n ->
      let fds = List.init n (fun x -> ExtUnix.All.file_descr_of_int (x + sd_listen_fds_start)) in
      List.iter Unix.set_close_on_exec fds;
      fds

  let listen_fds_lwt () =
    List.map Lwt_unix.of_unix_file_descr (listen_fds ())

  let get_args () =
    [
      ("-loglevel", Arg.String Log.set_loglevels, " ([<facil|prefix*>=]debug|info|warn|error[,])+");
      ExtArg.may_str "logfile" Daemon.logfile "<file> Log file";
      "-runas",
        Arg.String (fun name -> try Daemon.runas := Some (Unix.getpwnam name) with exn -> Exn.fail ~exn "runas: unknown user %s" name),
        "<user> run as specified user";
    ]

  let manage () =
    Daemon.foreground := true;
    Daemon.pidfile := None;
    Daemon.manage ()
end
