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

let handle_sig_exit fin =
  List.iter
    (fun signal -> Sys.set_signal signal (Sys.Signal_handle 
      (fun n ->
        Log.info "Received signal %i (exit)..." n;
        (try fin () with e -> Exn.log e "handle_sig_exit"); 
        Log.info "Signal handler done. Exiting.";
        exit 0)))
    [Sys.sigint; Sys.sigterm]

let handle_sig_reload f =
  List.iter
    (fun signal -> Sys.set_signal signal (Sys.Signal_handle 
      (fun n -> 
        Log.info "Received signal %i (reload)..." n; 
        (try f () with e -> Exn.log e "handle_sig_reload");
        Log.info "Signal handler done."
        )))
    [Sys.sighup; Sys.sigusr1; Sys.sigusr2]

