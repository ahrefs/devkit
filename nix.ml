(**
  *nix support
*)

open Unix

let unparent () =
  match fork () with  (* fork off and die *)
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

(*   umask 0o027; *)
(*   chdir "/"; *)
(*   redirect standard channels *)
  ()

let restart f x = let rec loop () = try f x with Unix.Unix_error (EINTR,_,_) -> loop () in loop ()

(** Use [with_sig_exit] *)
let handle_sig_exit_with fin =
  List.iter
    (fun signal -> Sys.set_signal signal (Sys.Signal_handle 
      (fun n ->
        Log.self #info "Received signal %i (exit)..." n;
        (try fin () with exn -> Log.self #warn ~exn "handle_sig_exit");
        Log.self #info "Signal handler done. Exiting.";
        exit 0)))
    [Sys.sigint; Sys.sigterm]

(** Use [with_sig_reload] *)
let handle_sig_reload_with fin =
  List.iter
    (fun signal -> Sys.set_signal signal (Sys.Signal_handle 
      (fun n -> 
        Log.self #info "Received signal %i (reload)..." n; 
        (try fin () with exn -> Log.self #warn ~exn "handle_sig_reload");
        Log.self #info "Signal handler done."
        )))
    [Sys.sighup; Sys.sigusr1; Sys.sigusr2]


type sig_stack = (unit -> unit) list ref

let sig_exit_funcs : sig_stack = ref []
let sig_reload_funcs : sig_stack = ref []

let register_sig st f k = Control.bracket (st := f :: !st) (fun () -> st := List.tl !st) k

let with_sig_exit f k = register_sig sig_exit_funcs f k
let with_sig_reload f k = register_sig sig_reload_funcs f k

let () = 
  handle_sig_exit_with (fun () ->
    List.iter (fun fin -> try fin () with exn -> Log.self #warn ~exn "sig_exit_funcs") !sig_exit_funcs);
  handle_sig_reload_with (fun () ->
    List.iter (fun fin -> try fin () with exn -> Log.self #warn ~exn "sig_reload_funcs") !sig_reload_funcs)

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> Printf.sprintf "unix:%s" s
  | Unix.ADDR_INET (addr,port) -> Printf.sprintf "%s:%u" (Unix.string_of_inet_addr addr) port

