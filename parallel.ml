
open Printf
open ExtLib
open Prelude

let log = Log.from "parallel"

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

module Threads(T:WorkerT) =
struct

type task = T.task
type result = T.result
type t = task Mtq.t * result Mtq.t * int

let worker qi f qo =
  while true do
    Mtq.put qo (f (Mtq.get qi))
  done

let stop ?wait:_ (qi,_,_) = Mtq.clear qi

let create f n =
  let qi = Mtq.create () and qo = Mtq.create () in
  for i = 1 to n do
    ignore (Thread.create (fun () -> worker qi f qo) ())
  done;
  qi,qo,n

let perform (qi,qo,n) ?autoexit:_ e f =
  let active = ref 0 in
  for i = 1 to n do
    match Enum.get e with
    | Some x -> Mtq.put qi x; incr active
    | None -> ()
  done;
  while !active > 0 do
    let res = Mtq.get qo in
    begin match Enum.get e with
    | Some x -> Mtq.put qi x
    | None -> decr active
    end;
    f res
  done

end

module Forks(T:WorkerT) =
struct

type task = T.task
type result = T.result
type instance = { mutable ch : (in_channel * out_channel) option; pid : int; }
type t = { mutable running : instance list; execute : (task -> result); mutable alive : bool; mutable gone : int; }

let worker (execute : task -> result) =
  let main_read, child_write = Unix.pipe () in
  let child_read, main_write = Unix.pipe () in
  match Lwt_unix.fork() with
  | -1 -> assert false
  | 0 -> (* child *)
      Unix.close main_read; Unix.close main_write;
      Unix.set_close_on_exec child_read;
      Unix.set_close_on_exec child_write;
      let output = Unix.out_channel_of_descr child_write in
      let input = Unix.in_channel_of_descr child_read in
      let rec loop () =
        let (r,_,e) = Nix.restart (fun () -> Unix.select [child_read] [] [child_read] (-1.)) () in
        assert (e=[]);
        assert (r<>[]);
        let v = try Some (Marshal.from_channel input : task) with End_of_file -> None in
        match v with
        | None -> ()
        | Some v ->
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
  | pid ->
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

open Unix

let close_ch w =
  match w.ch with
  | Some (cin,cout) -> w.ch <- None; close_in_noerr cin; close_out_noerr cout
  | None -> ()

let stop ?wait t =
  let reap l =
    List.filter_map (fun pid ->
    try
      if pid = fst (waitpid [WNOHANG] pid) then None (* exited *) else Some pid
    with
    | Unix_error (ECHILD,_,_) -> None (* exited *)
    | exn -> log #warn ~exn "Worker PID %d lost (wait)" pid; None) l
  in
  let hard_kill l =
      List.iter (fun pid ->
      try
        kill pid Sys.sigkill; log #warn "Worker PID %d killed with SIGKILL" pid
      with
      | Unix_error (ESRCH,_,_) -> ()
      | exn -> log #warn ~exn "Worker PID %d (SIGKILL)" pid) (reap l)
  in
  let gone () = if t.gone = 0 then "" else sprintf " (%d workers vanished)" t.gone in
  let rec reap_loop timeout l =
    match timeout, reap l with
    | _, [] -> log #info "Stopped %d workers properly%s" (List.length l) (gone ())
    | Some 0, l -> log #info "Timeouted, killing %d workers with SIGKILL%s" (List.length l) (gone ()); hard_kill l
    | _, l -> Nix.sleep 1.; reap_loop (Option.map pred timeout) l
  in
  log #info "Stopping %d workers%s" (List.length t.running) (gone ());
  t.alive <- false;
  let l = t.running |> List.map (fun w -> close_ch w; w.pid) in
  Nix.sleep 0.1; (* let idle workers detect EOF and exit peacefully (frequent io-in-signal-handler deadlock problem) *)
  l |> List.iter begin fun pid ->
    try kill pid Sys.sigterm with exn -> log #warn ~exn "Worker PID %d lost (SIGTERM)" pid
  end;
  t.running <- [];
  reap_loop wait l

let perform t ?(autoexit=false) e finish =
    match t.running with
    | [] -> Enum.iter (fun x -> finish (t.execute x)) e (* no workers *)
    | _ ->
      let workers = ref 0 in
      t.running |> List.iter begin fun w ->
        match w.ch with
        | None -> ()
        | Some (_,cout) ->
        match Enum.get e with
        | None -> ()
        | Some x -> incr workers; Marshal.to_channel cout (x : task) []; flush cout
      end;
(*       Printf.printf "workers %u\n%!" !workers; *)
      while !workers > 0 && t.alive do
        let fds = List.filter_map (function {ch=Some (cin,_); _} -> Some (Unix.descr_of_in_channel cin) | _ -> None) t.running in
        let (r,_,err) = Nix.restart (fun () -> Unix.select fds [] fds (-1.)) () in
        assert (err = []);
        let channels = r |> List.map (fun fd ->
          t.running |> List.find (function {ch=Some (cin,_); _} -> Unix.descr_of_in_channel cin = fd | _ -> false))
        in
        let answers = List.filter_map (fun w ->
          match w.ch with
          | None -> None
          | Some (cin,cout) ->
          let task = Enum.get e in
          try
            match try Some (Marshal.from_channel cin : result) with End_of_file -> None with
            | None ->
              log #warn "PID %d gone, what now?" w.pid;
              t.gone <- t.gone + 1;
              decr workers;
              (* close pipes and forget dead child, do not reap zombie so that premature exit is visible in process list *)
              close_ch w;
              t.running <- List.filter (fun w' -> w'.pid <> w.pid) t.running;
              None
            | Some answer ->
            begin match task with
            | None ->
              if autoexit then close_ch w;
              decr workers
            | Some x ->
              Marshal.to_channel cout (x : task) []; flush cout;
            end;
            Some answer
          with
          | exn -> log #warn ~exn "perform (from PID %d)" w.pid; decr workers; None)
        channels
        in
        List.iter finish answers;
      done;
      match t.gone with
      | 0 -> log #info "Finished"
      | n -> log #warn "Finished, %d workers vanished" n

end

let invoke (f : 'a -> 'b) x : unit -> 'b =
  let input, output = Unix.pipe() in
  match Lwt_unix.fork() with
  | -1 -> Unix.close input; Unix.close output; (let v = f x in fun () -> v)
  | 0 ->
      Unix.close input;
      let output = Unix.out_channel_of_descr output in
        Marshal.to_channel output (try `Res(f x) with e -> `Exn e) [];
        close_out output;
        exit 0
  | pid ->
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
  let f l = printf "got [%s]\n%!" (Util.strl Prelude.id l) in
  for i = 1 to 2 do
    W.perform workers (Enum.clone e) f;
    Thread.delay 1.
  done;
  print_endline "Done"
*)

let atomic_incr = incr
let atomic_decr = decr
let atomic_get x = !x

module ThreadPool = struct

  type t = { q : (unit -> unit) Mtq.t;
             total : int;
             free : int ref;
             mutable blocked : bool;
             }

  let create n =
    let t = { q = Mtq.create (); total = n; free = ref (-1); blocked = false;} in t

  let init t =
    let worker _i =
      while true do
        let f = Mtq.get t.q in
        atomic_decr t.free;
        begin try f () with exn -> log #warn ~exn "ThreadPool" end;
        atomic_incr t.free;
      done
    in
    t.free := t.total;
    for i = 1 to t.total do
      let (_:Thread.t) = Action.log_thread worker i in ()
    done

  let status t = Printf.sprintf "queue %d threads %d of %d"
                    (Mtq.length t.q) (atomic_get t.free) t.total

  let put t =
    if atomic_get t.free = -1 then init t;
    while t.blocked do
      Nix.sleep 0.05
    done;
    Mtq.put t.q

  let wait_blocked ?(n=0) t =
    if (atomic_get t.free <> -1) then begin
      while t.blocked do Nix.sleep 0.05 done;(* Wait for unblock *)
      t.blocked <- true;
      assert(n>=0);
      let i = ref 1 in
      while Mtq.length t.q + (t.total - atomic_get t.free)> n do (* Notice that some workers can be launched! *)
        if !i = 100 || !i mod 1000 = 0 then
          log #info "Thread Pool - waiting block : %s" (status t);
        Nix.sleep 0.05;
        incr i
      done;
      t.blocked <- false
    end

end

let rec launch_forks f = function
| [] -> ()
| x::xs ->
  match Lwt_unix.fork () with
  | 0 -> f x
  | -1 -> log #warn "failed to fork"
  | _ -> launch_forks f xs

module Thread = struct
type 'a t = [ `Exn of exn | `None | `Ok of 'a ] ref * Thread.t
let detach f x =
  let result = ref `None in
  result, Thread.create (fun () -> result := Exn.map f x) ()
let join (result,thread) = Thread.join thread; match !result with `None -> assert false | (`Ok _ | `Exn _ as x) -> x
let join_exn t = match join t with `Ok x -> x | `Exn exn -> raise exn
let map f a = Array.map join_exn @@ Array.map (detach f) a
let mapn ?(n=8) f l =
  assert (n > 0);
  Action.partition n l |> map (List.map @@ Exn.map f) |> Action.unpartition
end

let run_forks ?wait ?workers (type t) (f : t -> unit) l =
  let module Worker = struct type task = t type result = unit end in
  let module W = Forks(Worker) in
  let worker x =
    (* sane signal handler FIXME restore? *)
    Signal.set_exit Daemon.signal_exit;
    f x
  in
  let proc = W.create worker (match workers with Some n -> assert (n > 0); n | None -> List.length l) in
  Nix.handle_sig_exit_with ~exit:true (fun () -> W.stop ?wait proc); (* FIXME: output in signal handler *)
  W.perform ~autoexit:true proc (List.enum l) id;
  W.stop proc

let run_forks' f l =
  match l with
  | [] -> ()
  | [x] -> f x
  | l -> run_forks f l
