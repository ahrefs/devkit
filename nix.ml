(**
  *nix support
*)

open Unix
open Printf

open Control
open Prelude
open ExtLib

let log = Log.from "nix"

let fork () =
  match Lwt_unix.fork () with
  | -1 -> Exn.fail "failed to fork"
  | 0 -> Random.self_init (); Pid.update (); `Child
  | pid -> `Forked pid

(** fork off and die *)
let unparent () =
  match fork () with
  | `Child -> ()
  | `Forked _ -> exit 0

(*
  http://www.itp.uzh.ch/~dpotter/howto/daemonize
*)

let daemonize () =
  unparent ();
  if setsid () < 0 then failwith "Can't setsid";

  (* Ignore TTY signals, SIGHUP and SIGPIPE *)
  List.iter (fun n -> Sys.set_signal n Sys.Signal_ignore) [Sys.sigtstp; Sys.sigttou; Sys.sigttin; Sys.sighup; Sys.sigpipe];

(*   umask 0; *) (* TODO investigate *)
(*   chdir "/"; *) (* TODO this will break lots of code - fix *)

  (* redirect standard channels *)
  let null = openfile "/dev/null" [O_RDWR] 0 in
  dup2 null stdin;
  dup2 null stdout;
  dup2 null stderr;
  close null;
  ()

let write_pidfile path =
  Control.bracket (open_out_gen [Open_wronly;Open_creat;Open_excl;Open_text] 0o644 path) close_out begin fun ch ->
    try
      Printf.fprintf ch "%u\n" (getpid ());
      flush ch
    with
      exn -> log #warn ~exn "cannot write pidfile %s, will remove" path; Sys.remove path; raise exn
  end

let read_pidfile path =
  Control.with_open_in_txt path (fun ch -> let ib = Scanf.Scanning.from_channel ch in Scanf.bscanf ib " %u " Prelude.id)

let probe_pidfile path =
  if Sys.file_exists path then
    try
      let pid = read_pidfile path in
      kill pid 0;
      `Alive pid
    with
    | Unix_error (ESRCH, _, _) -> `Stale
    | e -> `Error e
  else
    `Missing

let check_pidfile path =
  match probe_pidfile path with
  | `Missing -> () (* free to go *)
  | `Stale -> log #info "removing stale pidfile at %s" path; Sys.remove path
  | `Alive pid -> log #info "pid %d at %s is alive, exiting" pid path; exit 133
  | `Error exn -> log #warn ~exn "wrong pid file at %s, exiting" path; exit 3

let manage_pidfile path =
  check_pidfile path;
  write_pidfile path;
  let pid = getpid () in
  at_exit (fun () -> if getpid () = pid then Exn.suppress Sys.remove path (* else forked child *))

let restart f x = let rec loop () = try f x with Unix.Unix_error (EINTR,_,_) -> loop () in loop ()

(** NB be careful with mutexes in signal handlers.
    Outputting anything to ocaml channels is a potential deadlock.
    Use signalfd which invokes signal handlers in predictable context.
    @deprecated easy to deadlock, use signalfd instead
*)
let handle_sig_exit_with ~exit fin =
  List.iter
    (fun signal -> Sys.set_signal signal (Sys.Signal_handle
      (fun _signo ->
(*         log #info "Received signal %i (exit)..." n; *)
        (try fin () with exn -> log #warn ~exn "handle_sig_exit");
(*         log #info "Signal handler done.%s" (if exit then " Exiting." else ""); *)
        if exit then Stdlib.exit 0)))
    [Sys.sigint; Sys.sigterm]

(**
  @deprecated easy to deadlock, use signalfd instead
*)
let handle_sig_reload_with fin =
  List.iter
    (fun signal -> Sys.set_signal signal (Sys.Signal_handle
      (fun _signo ->
(*         log #info "Received signal %i (reload)..." n;  *)
        (try fin () with exn -> log #warn ~exn "handle_sig_reload");
(*         log #info "Signal handler done." *)
        )))
    [Sys.sighup]

let show_addr = function
  | ADDR_UNIX s -> sprintf "unix:%s" s
  | ADDR_INET (addr,port) -> sprintf "%s:%u" (string_of_inet_addr addr) port

let get_inet_addr_exn = function
  | ADDR_INET (addr,_) -> addr
  | addr -> Exn.fail "get_inet_addr %s" (show_addr addr)

let show_inet_addr_exn addr = string_of_inet_addr (get_inet_addr_exn addr)

let make_inet_addr_exn host port =
  let a = (gethostbyname host).h_addr_list in
  if Array.length a = 0 then Exn.fail "make_inet_addr %s %d" host port else
  ADDR_INET (a.(0), port)

let inet_addr_of_string s =
  let open Unix in
  try
    if String.contains s ':' then
      let (host, port) = String.split s ":" in
      let port = int_of_string port in
      match host with
      | "*" -> ADDR_INET (inet_addr_any, port)
      | host -> make_inet_addr_exn host port
    else
      let port = int_of_string s in
      ADDR_INET (inet_addr_loopback, port)
  with _ -> (* The port or the host is invalid *)
    Exn.fail "invalid INET addr %S" s

let unix_addr_of_string s =
  let open Unix in
  if Stre.starts_with s "unix:" then
    ADDR_UNIX (String.slice ~first:5 s)
  else
    Exn.fail "invalid UNIX addr %S" s

let parse_addr_port_exn s =
  match Stre.splitc s ':' with
  | exception Not_found ->
      Exn.fail "bad host in %S (must be host:port)" s
  | host, port ->
      let port = try int_of_string port with exn -> Exn.fail ~exn "bad port %s in %S" port s in
      (host, port)

(** Parse input as [ip:port]
  @return a tuple representing ip and port *)
let parse_ip_port_exn s =
  let ip, port = parse_addr_port_exn s in
  let ip = try Unix.inet_addr_of_string ip with exn -> Exn.fail ~exn "bad ip %s in %S" ip s in
  (ip, port)

(**
   Convert a string to a {Unix.sockaddr}.

   Formats supported are:
   - unix:file_path
   - host:port
   - *:port, using {Unix.inet_addr_any}
   - port, using {Unix.inet_addr_loopback}
 *)
let sockaddr_of_string s =
  try unix_addr_of_string s
  with Failure _ ->
  try inet_addr_of_string s
  with Failure _ ->
    Exn.fail "sockaddr_of_string %s" s

(** Execute process and capture stdout to string, @return empty string on error *)
let read_process cmd =
  try
    let cin = Unix.open_process_in cmd in
    let input = IO.input_channel cin in
    let data = IO.read_all input in
    IO.close_in input;
    ignore (Unix.close_process_in cin);
    data
  with _ -> ""

module Ev = Libevent

(** @return IO.t to feed stdin of spawned process *)
let output_process_exit cmd =
  let cout = Unix.open_process_out cmd in
  let close () = Unix.close_process_out cout in
  IO.create_out
    ~write:(output_char cout)
    ~output:(fun s o l -> output cout s o l; l)
    ~flush:(fun () -> flush cout)
    ~close

(** @return IO.t to feed stdin of spawned process *)
let output_process cmd =
  let cout = Unix.open_process_out cmd in
  let close () =
    match Unix.close_process_out cout with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED n -> Exn.fail "Command \"%s\": Exit code %u" cmd n
    | Unix.WSIGNALED n | Unix.WSTOPPED n -> Exn.fail "Command \"%s\": Terminated with signal %u" cmd n
  in
  IO.create_out
    ~write:(output_char cout)
    ~output:(fun s o l -> output cout s o l; l)
    ~flush:(fun () -> flush cout)
    ~close

let write_process_exn cmd data =
  with_output (output_process cmd) (fun out -> IO.nwrite out data; IO.flush out)

let write_process cmd data = try write_process_exn cmd data; true with _ -> false

let mounts () =
  with_open_in_txt "/proc/mounts" @@ fun ch ->
  Std.input_lines ch |>
  Enum.filter_map (fun s ->
    match String.nsplit s " " with
    | ["rootfs";_;"rootfs";_;_;_] -> None
    | [dev;mount;_fs;opt;_;_] -> Some (dev, mount, String.nsplit opt ",")
    | _ -> Exn.fail "bad mount : %s" s) |>
  List.of_enum

(** @param path must be normalized *)
let find_mount path =
  assert (not @@ Filename.is_relative path);
  assert (not @@ Stre.exists path "//");
  assert (not @@ Stre.exists path "/./");
  assert (not @@ Stre.exists path "/../");
  let mount = ref ("","",[]) in
  let bound x = let (_,b,_) = x in b in
  mounts () |>
  List.iter (fun (_,bind,_ as part) ->
    if Stre.starts_with path bind && String.length bind > String.length (bound !mount) then
      mount := part);
  assert (bound !mount <> "");
  !mount

(* in seconds *)
let sleep = Unix.sleepf

(**
  Buffered output to [Unix.file_descr].
  Doesn't own the file descriptor.
*)
let output_buf_fd ?(bufsize=1*1024*1024) fd =
  if bufsize <= 0 then Exn.invalid_arg "output_fd: bufsize %d" bufsize;
  let buf = Bytes.create bufsize in
  let len = ref 0 in
  let flush () =
    match !len with
    | 0 -> ()
    | _ ->
      let written = Unix.write fd buf 0 !len in
      if !len <> written then Exn.fail "output_fd: flush failed: %d <> %d" !len written;
      len := 0
  in
  let check_flush () = if !len = bufsize then flush () in
  let rec output s p l =
    if l + !len > bufsize then
    begin
      let miss = bufsize - !len in
      Bytes.blit s p buf !len miss;
      len := bufsize;
      flush ();
      output s (p + miss) (l - miss)
    end else begin
      Bytes.blit s p buf !len l;
      len := !len + l;
      check_flush ()
    end
  in
  IO.create_out ~write:(fun c -> Bytes.set buf !len c; incr len; check_flush ())
  ~output:(fun s p l -> output s p l; l)
  ~flush
  ~close:flush (* do not close file descriptor, flush the buffer *)

let unlimit_soft r =
  let (soft,hard) = U.getrlimit r in
  try
    U.setrlimit r ~soft:hard ~hard
  with Unix_error ((EPERM|EINVAL as error),_,_) when r = U.RLIMIT_NOFILE ->
    log #warn "failed to unlimit NOFILE %s -> %s : %s (check kernel limits fs.nr_open/kern.maxfilesperproc/etc), ignored"
      (U.Rlimit.to_string soft) (U.Rlimit.to_string hard) (error_message error)

(** raise core and nofile soft limits (to the corresponding hard limits) *)
let raise_limits () =
  unlimit_soft U.RLIMIT_CORE;
  unlimit_soft U.RLIMIT_NOFILE

let connect fd sockaddr =
  let open Unix in
  try connect fd sockaddr with Unix_error (e, f, "") -> raise (Unix_error (e, f, show_addr sockaddr))

let connect_lwt fd sockaddr =
  let open Lwt_unix in
  Lwt.catch
    (fun () -> connect fd sockaddr)
    (function Unix_error (e, f, "") -> raise (Unix_error (e, f, show_addr sockaddr)) | exn -> Lwt.reraise exn)

let get_xdg_dir ~env dir =
  try Sys.getenv env with Not_found ->
  try sprintf "%s/.%s" (Sys.getenv "HOME") dir with Not_found ->
    Exn.fail "get_xdg_dir: unable to find %s directory" dir

let xdg_cache_dir = lazy (get_xdg_dir ~env:"XDG_DATA_CACHE" "cache")
let xdg_config_dir = lazy (get_xdg_dir ~env:"XDG_CONFIG_HOME" "config")

let quote_if_needed s = try Scanf.sscanf s "%_[a-zA-Z0-9:_/.-]%!" s with _ -> Filename.quote s

let args = List.tl (Array.to_list Sys.argv) (* Action.args *)
let cmdline = String.concat " " @@ List.map quote_if_needed @@ Array.to_list Sys.argv
