(**
  *nix support
*)

open Unix

open Control
open Prelude
open ExtLib

let unparent () =
  match Lwt_unix.fork () with  (* fork off and die *)
  | 0 -> ()
  | -1 -> failwith "Fork failed"
  | _ -> exit 0

(*
  http://www-theorie.physik.unizh.ch/~dpotter/howto/daemonize
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
  Control.with_open_out_txt path (fun ch -> Printf.fprintf ch "%u\n" (getpid ()))

let read_pidfile path =
  Control.with_open_in_txt path (fun ch -> Scanf.fscanf ch " %u " Prelude.id)

let probe_pidfile path =
  if Sys.file_exists path then
    try
      let pid = read_pidfile path in
      kill pid 0;
      `Alive
    with
    | Unix_error (ESRCH, _, _) -> `Stale
    | e -> `Error e
  else
    `Missing

let check_pidfile path =
  match probe_pidfile path with
  | `Missing -> () (* free to go *)
  | `Stale -> Log.self #info "removing stale pidfile"; Exn.suppress Sys.remove path
  | `Alive -> Log.self #info "pid is alive, exiting"; exit 2
  | `Error exn -> Log.self #warn ~exn "wrong pid file, exiting"; exit 3

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
(*         Log.self #info "Received signal %i (exit)..." n; *)
        (try fin () with exn -> Log.self #warn ~exn "handle_sig_exit");
(*         Log.self #info "Signal handler done.%s" (if exit then " Exiting." else ""); *)
        if exit then Pervasives.exit 0)))
    [Sys.sigint; Sys.sigterm]

(**
  @deprecated easy to deadlock, use signalfd instead
*)
let handle_sig_reload_with fin =
  List.iter
    (fun signal -> Sys.set_signal signal (Sys.Signal_handle 
      (fun _signo -> 
(*         Log.self #info "Received signal %i (reload)..." n;  *)
        (try fin () with exn -> Log.self #warn ~exn "handle_sig_reload");
(*         Log.self #info "Signal handler done." *)
        )))
    [Sys.sighup]

let show_addr = function
  | ADDR_UNIX s -> Printf.sprintf "unix:%s" s
  | ADDR_INET (addr,port) -> Printf.sprintf "%s:%u" (string_of_inet_addr addr) port

let make_inet_addr_exn host port =
  let a = (gethostbyname host).h_addr_list in
  if Array.length a = 0 then Exn.fail "make_inet_addr %s %d" host port else
  ADDR_INET (a.(0), port)

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

module UnixImpl = struct

open Unix

(* FD_CLOEXEC should be supported on all Unix systems these days,
   but just in case... *)
let try_set_close_on_exec fd =
  try set_close_on_exec fd; true with Invalid_argument _ -> false

let open_proc cmd input output toclose =
  let cloexec = List.for_all try_set_close_on_exec toclose in
  match Lwt_unix.fork () with
  |  0 -> if input <> stdin then begin dup2 input stdin; close input end;
          if output <> stdout then begin dup2 output stdout; close output end;
          if not cloexec then List.iter close toclose;
          U.setpgid 0 0; (* separate process group *)
          begin try execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
          with _ -> exit 127
          end
  | id -> id

let open_process_in cmd =
  let (in_read, in_write) = pipe() in
  let inchan = in_channel_of_descr in_read in
  let pid = open_proc cmd stdin in_write [in_read] in
  close in_write;
  inchan, pid

let close_process_in (cin,pid) = 
  close_in cin;
  restart (waitpid []) pid

end (* module UnixImpl *)

(*
let read_process_exn ?timeout cmd =
  bracket (UnixImpl.open_process_in cmd) (ignore $ UnixImpl.close_process_in) & fun (cin, pid) ->
  let fd = Unix.descr_of_in_channel cin in
  Unix.set_nonblock fd;
  bracket (Ev.init ()) Ev.free & fun base ->
  let ok = ref false in
  let b = Buffer.create 16 in
  let ev = Ev.create () in
  Ev.set base ev fd [Ev.READ] ~persist:true (fun fd flags ->
    let fin b = (* Ev.del called from inside event loop to break it *) Ev.del ev; ok := b in
    try
    if flags = Ev.TIMEOUT then
    begin
      Unix.kill (-pid) Sys.sigkill; (* kill the whole process group *)
      fin false
    end
    else
      match Async.read_available ~limit:max_int fd with
      | `Done s -> Buffer.add_string b s; fin true
      | `Limit q -> assert false
      | `Part s -> Buffer.add_string b s
    with
      exn -> Log.self#warn ~exn "event"; fin false);
  Ev.add ev timeout;
  Ev.dispatch base;
  if !ok then
    Some (Buffer.contents b)
  else
    None
*)

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
  with_open_in_txt "/proc/mounts" & fun ch ->
  Std.input_lines ch >>
  Enum.filter_map (fun s ->
    match String.nsplit s " " with
    | ["rootfs";_;"rootfs";_;_;_] -> None
    | [dev;mount;_fs;opt;_;_] -> Some (dev, mount, String.nsplit opt ",")
    | _ -> Exn.fail "bad mount : %s" s) >>
  List.of_enum

(** @param path must be normalized *)
let find_mount path =
  assert (not & Filename.is_relative path);
  assert (not & String.exists path "//");
  assert (not & String.exists path "/./");
  assert (not & String.exists path "/../");
  let mount = ref ("","",[]) in
  let bound x = let (_,b,_) = x in b in
  mounts () >>
  List.iter (fun (_,bind,_ as part) -> 
    if String.starts_with path bind && String.length bind > String.length (bound !mount) then
      mount := part);
  assert (bound !mount <> "");
  !mount

module Mallinfo = struct

type t = {
 arena : int;    (* non-mmapped space allocated from system *)
 ordblks : int;  (* number of free chunks *)
 smblks : int;   (* number of fastbin blocks *)
 hblks : int;    (* number of mmapped regions *)
 hblkhd : int;   (* space in mmapped regions *)
 usmblks : int;  (* maximum total allocated space *)
 fsmblks : int;  (* space available in freed fastbin blocks *)
 uordblks : int; (* total allocated space *)
 fordblks : int; (* total free space *)
 keepcost : int; (* top-most, releasable (via malloc_trim) space *)
}

let to_string v =
  Printf.sprintf "arena %d ordblks %d smblks %d hblks %d hblkhd %d usmblks %d fsmblks %d uordblks %d fordblks %d keepcost %d"
    v.arena v.ordblks v.smblks v.hblks v.hblkhd v.usmblks v.fsmblks v.uordblks v.fordblks v.keepcost

end

(* in seconds *)
let sleep = restart Thread.delay

(**
  Buffered output to [Unix.file_descr].
  Doesn't own the file descriptor.
*)
let output_buf_fd ?(bufsize=1*1024*1024) fd =
  if bufsize <= 0 then Exn.invalid_arg "output_fd: bufsize %d" bufsize;
  let buf = String.create bufsize in
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
      String.blit s p buf !len miss;
      len := bufsize;
      flush ();
      output s (p + miss) (l - miss)
    end else begin
      String.blit s p buf !len l;
      len := !len + l;
      check_flush ()
    end
  in
  IO.create_out ~write:(fun c -> buf.[!len] <- c; incr len; check_flush ())
  ~output:(fun s p l -> output s p l; l)
  ~flush
  ~close:flush (* do not close file descriptor, flush the buffer *)

let unlimit_soft r = let (_,hard) = U.getrlimit r in U.setrlimit r ~soft:hard ~hard

(** raise core and nofile soft limits (to the corresponding hard limits) *)
let raise_limits () =
  unlimit_soft U.RLIMIT_CORE;
  unlimit_soft U.RLIMIT_NOFILE
