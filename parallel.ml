open Printf
open ExtLib
open Prelude

let log = Log.from "parallel"

type revive_mode =
  | Never
  | On_failure
  | Always

module type WorkerT = sig
  type task
  type result
end

module type Workers = sig
type task
type result
type t
val create : (task -> result) -> int -> t
val perform : t -> ?autoexit:bool -> task Enum.t -> (result -> unit) -> unit
val stop : ?wait:int -> t -> unit
end

(** @return list of reaped PIDs with status, list of live PIDs *)
let reap l =
  let open Unix in
  List.fold_left begin fun (acc_dead, acc_alive) pid ->
    let dead result = (pid, result) :: acc_dead, acc_alive in
    let alive () = acc_dead, pid :: acc_alive in
    match waitpid [WNOHANG] pid with
    | 0, _ ->
      (* due to NOHANG *)
      alive ()
    | v, _ when v != pid ->
      log #warn "Unexpected waitpid value %d for PID %d" v pid;
      alive ()
    | v, result ->
      assert (v = pid);
      dead (Some result)
    | exception Unix_error (ECHILD,_,_) ->
      dead None
      (* exited *)
    | exception exn ->
      log #warn ~exn "Worker PID %d lost (wait)" pid;
      dead None
  end ([], []) l

let hard_kill1 pid =
  let open Unix in
  try
    kill pid Sys.sigkill; log #warn "Worker PID %d killed with SIGKILL" pid
  with
  | Unix_error (ESRCH,_,_) -> ()
  | exn -> log #warn ~exn "Worker PID %d (SIGKILL)" pid

let hard_kill l =
  let (_,live) = reap l in
  List.iter hard_kill1 live

let killall signo pids =
  pids |> List.iter begin fun pid ->
    try Unix.kill pid signo with exn -> log #warn ~exn "PID %d lost (trying to send signal %d)" pid signo
  end

let do_stop ?wait pids =
  let rec reap_loop timeout l =
    let (_,live) = reap l in
    match timeout, live with
    | _, [] -> `Done
    | Some 0, l -> hard_kill l; `Killed (List.length l)
    | _, l -> Nix.sleep 1.; reap_loop (Option.map pred timeout) l
  in
  killall Sys.sigterm pids;
  reap_loop wait pids

module Forks(T:WorkerT) =
struct

type task = T.task
type result = T.result
type instance = { mutable ch : (in_channel * out_channel) option; pid : int; }
type t = { mutable running : instance list; execute : (task -> result); mutable alive : bool; mutable gone : int; }

let worker (execute : task -> result) =
  let main_read, child_write = Unix.pipe () in
  let child_read, main_write = Unix.pipe () in
  match Nix.fork () with
  | `Child -> (* child *)
      Unix.close main_read; Unix.close main_write;
      Unix.set_close_on_exec child_read;
      Unix.set_close_on_exec child_write;
      let output = Unix.out_channel_of_descr child_write in
      let input = Unix.in_channel_of_descr child_read in
      let rec loop () =
        match Nix.restart (fun () -> ExtUnix.All.(poll [| child_read, Poll.(pollin + pollpri); |] (-1.))) () with
        | [] | _ :: _ :: _ -> assert false
        | [ _fd, revents; ] ->
        assert (not (ExtUnix.All.Poll.(is_set revents pollpri)));
        assert (ExtUnix.All.Poll.(is_inter revents (pollin + pollhup)));
        match (Marshal.from_channel input : task) with
        | exception End_of_file -> ()
        | exception exn -> log #error ~exn "Parallel.worker failed to unmarshal task"
        | v ->
          let r = execute v in
          Marshal.to_channel output (r : result) [];
          flush output;
          loop ()
      in
      begin try
        loop ()
      with exn ->
        log #error ~exn ~backtrace:true "Parallel.worker aborting on uncaught exception"
      end;
      close_in_noerr input;
      close_out_noerr output;
      exit 0
  | `Forked pid ->
      Unix.close child_read; Unix.close child_write;
      (* prevent sharing these pipes with other children *)
      Unix.set_close_on_exec main_write;
      Unix.set_close_on_exec main_read;
      let cout = Unix.out_channel_of_descr main_write in
      let cin = Unix.in_channel_of_descr main_read in
      { ch = Some (cin, cout); pid; }

let create execute n =
  let running = List.init n (fun _ -> worker execute) in
  { running; execute; alive=true; gone=0; }

let close_ch w =
  match w.ch with
  | Some (cin,cout) -> w.ch <- None; close_in_noerr cin; close_out_noerr cout
  | None -> ()

let stop ?wait t =
  let gone () = if t.gone = 0 then "" else sprintf " (%d workers vanished)" t.gone in
  log #info "Stopping %d workers%s" (List.length t.running) (gone ());
  t.alive <- false;
  let l = t.running |> List.map (fun w -> close_ch w; w.pid) in
  Nix.sleep 0.1; (* let idle workers detect EOF and exit peacefully (frequent io-in-signal-handler deadlock problem) *)
  t.running <- [];
  match do_stop ?wait l with
  | `Done -> log #info "Stopped %d workers properly%s" (List.length l) (gone ())
  | `Killed killed -> log #info "Timeouted, killing %d (of %d) workers with SIGKILL%s" killed (List.length l) (gone ())

let perform t ?(autoexit=false) tasks finish =
    match t.running with
    | [] -> Enum.iter (fun x -> finish (t.execute x)) tasks (* no workers *)
    | _ ->
      let workers = ref 0 in
      t.running |> List.iter begin fun w ->
        match w.ch with
        | None -> ()
        | Some (_,cout) ->
        match Enum.get tasks with
        | None -> ()
        | Some x -> incr workers; Marshal.to_channel cout (x : task) []; flush cout
      end;
(*       Printf.printf "workers %u\n%!" !workers; *)
      let events = ExtUnix.All.Poll.(pollin + pollpri) in
      while !workers > 0 && t.alive do
        let fds = List.filter_map (function {ch=Some (cin,_); _} -> Some (Unix.descr_of_in_channel cin) | _ -> None) t.running in
        let r = Nix.restart (fun () -> ExtUnix.All.poll (Array.of_list (List.map (fun fd -> fd, events) fds)) (-1.)) () in
        assert (not (List.exists (fun (_fd, revents) -> ExtUnix.All.Poll.(is_set revents pollpri)) r));
        let channels = r |> List.map (fun (fd, _revents) ->
          t.running |> List.find (function {ch=Some (cin,_); _} -> Unix.descr_of_in_channel cin = fd | _ -> false))
        in
        let answers = channels |> List.filter_map begin fun w ->
          match w.ch with
          | None -> None
          | Some (cin,cout) ->
          try
            match (Marshal.from_channel cin : result) with
            | exception exn ->
              log #warn ~exn "no result from PID %d" w.pid;
              t.gone <- t.gone + 1;
              decr workers;
              (* close pipes and forget dead child, do not reap zombie so that premature exit is visible in process list *)
              close_ch w;
              t.running <- List.filter (fun w' -> w'.pid <> w.pid) t.running;
              None
            | answer ->
              begin match Enum.get tasks with
              | None ->
                if autoexit then close_ch w;
                decr workers
              | Some x ->
                Marshal.to_channel cout (x : task) []; flush cout;
              end;
              Some answer
          with
          | exn -> log #warn ~exn "perform (from PID %d)" w.pid; decr workers; None
        end
        in
        List.iter finish answers;
      done;
      match t.gone with
      | 0 -> log #info "Finished"
      | n -> log #warn "Finished, %d workers vanished" n

end

let invoke (f : 'a -> 'b) x : unit -> 'b =
  let input, output = Unix.pipe() in
  match Nix.fork () with
  | exception _ -> Unix.close input; Unix.close output; (let v = f x in fun () -> v)
  | `Child ->
      Unix.close input;
      let output = Unix.out_channel_of_descr output in
        Marshal.to_channel output (try `Res(f x) with e -> `Exn e) [];
        close_out output;
        exit 0
  | `Forked pid ->
      Unix.close output;
      let input = Unix.in_channel_of_descr input in fun () ->
        let v = Marshal.from_channel input in
        ignore (Nix.restart (Unix.waitpid []) pid);
        close_in input;
        match v with `Res x -> x | `Exn e -> raise e

(*

(* example *)
open Printf

module W = Workers(struct type task = string type result = string list end)

let execute s = for i = 1 to 100_000 do Thread.delay 0. done; printf "%u : %s\n%!" (Unix.getpid()) s; [s;s;s;s]

let () =
  let workers = W.create execute 4 in
  print_endline "go";
  let e = Enum.init 100 (sprintf "<%u>") in
  let f l = printf "got [%s]\n%!" (Stre.list Prelude.id l) in
  for i = 1 to 2 do
    W.perform workers (Enum.clone e) f;
    Thread.delay 1.
  done;
  print_endline "Done"
*)

let rec launch_forks f = function
| [] -> ()
| x::xs ->
  match Nix.fork () with
  | `Child -> f x
  | `Forked _ -> launch_forks f xs

(** keep the specifed number of workers running *)
let run_forks_simple ?(revive=Never) ?wait_stop f args =
  let workers = Hashtbl.create 1 in
  let launch f x =
    match Nix.fork () with
    | `Child ->
      begin try
        f x;
        exit 0
      with exn ->
        log #error ~exn ~backtrace:true "worker failed";
        exit 1
      end
    | `Forked pid -> Hashtbl.add workers pid x; pid
  in
  args |> List.iter (fun x -> let (_:int) = launch f x in ());
  let pids () = Hashtbl.keys workers |> List.of_enum in
  let maybe_revive ~always dead =
    dead |> List.iter begin fun (pid, result) ->
      match Hashtbl.find workers pid with
      | exception Not_found -> log #warn "WUT? Not my worker %d" pid
      | x ->
      Hashtbl.remove workers pid;
      match result with
      | Some (Unix.WEXITED 0) when not always ->
        (* do not relaunch *)
        log #info "worker %d exited" pid;
      | _ ->
      match launch f x with
      | exception exn -> log #error ~exn "restart"
      | pid' -> log #info "worker %d exited%s, replaced with %d" pid (if always then "" else "with non-zero status") pid';
    end
  in
  let rec loop pause =
    Nix.sleep pause;
    let total = Hashtbl.length workers in
    if total = 0 && revive <> Always then
      log #info "All workers dead, stopping"
    else
    match Daemon.should_exit () with
    | true ->
      log #info "Stopping %d workers" total;
      begin match do_stop ?wait:wait_stop (Hashtbl.keys workers |> List.of_enum) with
      | `Done -> log #info "Stopped %d workers" total
      | `Killed n -> log #info "Killed %d (of %d) workers with SIGKILL" n total
      end
    | false ->
    let (dead,_live) = reap (pids ()) in
    match dead, revive with
    | [], _ -> loop (max 1. (pause /. 2.))
    | dead, Always ->
      maybe_revive ~always:true dead;
      loop (min 10. (pause *. 1.5))
    | dead, On_failure ->
      maybe_revive ~always:false dead;
      loop (min 10. (pause *. 1.5))
    | dead, Never ->
      log #info "%d child workers exited (PIDs: %s)" (List.length dead) (Stre.list (string_of_int $ fst) dead);
      List.iter (Hashtbl.remove workers $ fst) dead;
      loop pause
  in
  loop 1.

let run_workers_enum workers ?wait_stop (type t) (type u) (f : t -> u) (g : u -> unit) enum =
  assert (workers > 0);
  let module Worker = struct type task = t type result = u end in
  let module W = Forks(Worker) in
  let worker x =
    (* sane signal handler FIXME restore? *)
    Signal.set_exit Daemon.signal_exit;
    f x
  in
  let proc = W.create worker workers in
  Nix.handle_sig_exit_with ~exit:true (fun () -> W.stop ?wait:wait_stop proc); (* FIXME: output in signal handler *)
  W.perform ~autoexit:true proc enum g;
  W.stop proc

let run_workers workers ?wait_stop (type t) (f : t -> unit) l =
  run_workers_enum workers ?wait_stop f id (List.enum l)

let run_forks ?wait_stop ?revive ?wait ?workers (type t) (f : t -> unit) l =
  let wait_stop = if wait_stop = None then wait else wait_stop in
  match workers with
  | None -> run_forks_simple ?wait_stop ?revive f l
  | Some n -> run_workers n ?wait_stop f l

let run_forks' f l =
  match l with
  | [] -> ()
  | [x] -> f x
  | l -> run_forks f l

module Services = struct
  type t = {
    mutable pids : int list;
    work : int -> unit Lwt.t;
  }

  let start n work =
    let rec start_forked i =
      if i >= n then Lwt.return_nil
      else (
        match Nix.fork () with
        | `Child ->
          let%lwt () = work i in
          exit 0
        | `Forked pid ->
          log#debug "Starting worker %d with pid %d" i pid;
          Lwt.map (fun pids -> pid :: pids) (start_forked (i + 1))
      )
    in
    Lwt.map (fun pids -> { pids; work }) (start_forked 0)

  let wait pid =
    try%lwt Lwt.map fst (Lwt_unix.waitpid [] pid) with
    | Unix.Unix_error (ECHILD, _, _) -> Lwt.return pid
    | exn ->
      log#warn ~exn "Worker PID %d lost (wait)" pid;
      Lwt.return pid

  let kill ~timeout pid =
    let graceful =
      Unix.kill pid Sys.sigterm;
      let%lwt _ = wait pid in
      log#debug "Worker PID %d killed with SIGTERM" pid;
      Lwt.return_unit
    in
    let ungraceful =
      let%lwt () = Lwt_unix.sleep timeout in
      hard_kill1 pid;
      Lwt.return_unit
    in
    Lwt.pick [ graceful; ungraceful ]

  let rolling_restart ?wait ~timeout workers =
    let%lwt pids =
      Lwt_list.mapi_s begin fun i pid ->
        log#debug "Restarting worker %d with PID %d\n%!" i pid;
        let%lwt () = kill ~timeout pid in
        Option.may Unix.sleep wait;
        match Nix.fork () with
        | `Child ->
           let%lwt () = workers.work i in
           exit 0
        | `Forked pid' ->
           log#debug "Worker %d started with PID %d\n%!" i pid';
           Lwt.return pid'
      end
      workers.pids
    in
    workers.pids <- pids;
    Lwt.return_unit

  let stop ~timeout { pids; _ } =
    log#info "Stopping workers";
    Lwt_list.iteri_p begin fun i pid ->
      log#debug "Stopping worker %d with PID %d" i pid;
      kill ~timeout pid
    end pids
end
