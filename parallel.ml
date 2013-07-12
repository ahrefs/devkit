
open ExtLib

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
val perform : t -> task Enum.t -> (result -> unit) -> unit
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

let stop ?wait (qi,_,_) = Mtq.clear qi

let create f n =
  let qi = Mtq.create () and qo = Mtq.create () in
  for i = 1 to n do
    ignore (Thread.create (fun () -> worker qi f qo) ())
  done;
  qi,qo,n

let perform (qi,qo,n) e f =
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

let worker (execute : task -> result) =
  let main_read, child_write = Unix.pipe () in
  let child_read, main_write = Unix.pipe () in
  match Unix.fork() with
  | -1 -> assert false
  | 0 -> (* child *)
      Unix.close main_read; Unix.close main_write;
      let output = Unix.out_channel_of_descr child_write in
      let input = Unix.in_channel_of_descr child_read in
      begin try
      while true do
(*         log #info "wait"; *)
        let (r,_,e) = Nix.restart (fun () -> Unix.select [child_read] [] [child_read] (-1.)) () in
        assert (e=[]);
        assert (r<>[]);
(*         log #info "wait done, read"; *)
        let v = (Marshal.from_channel input : task) in
(*         log #info "read done, execute"; *)
        let r = execute v in
(*         log #info "execute done, write"; *)
        Marshal.to_channel output (r : result) []; flush output;
(*         log #info "write done" *)
      done
      with exn ->
        log #error ~exn ~backtrace:true "Paraller.worker aborting on uncaught exception"
      end;
      close_in_noerr input;
      close_out_noerr output;
      exit 0
  | pid ->
      Unix.close child_read; Unix.close child_write;
      let output = Unix.out_channel_of_descr main_write in
      let input = Unix.in_channel_of_descr main_read in
      input,output,pid

type t = (in_channel * out_channel * int) list ref * (task -> result) * bool ref

let create f n = let l = ref [] in for i = 1 to n do l := worker f :: !l done; l, f, ref true

open Unix

let stop ?wait (l,_,alive) =
  let reap l =
    List.filter_map (fun pid ->
    try 
      if pid = fst (waitpid [WNOHANG] pid) then None (* exited *) else Some pid 
    with 
    | Unix_error (ECHILD,_,_) -> None (* exited *)
    | exn -> log #warn "Worker PID %d lost (wait)" pid; None) l
  in
  let hard_kill l =
      List.iter (fun pid -> 
      try 
        kill pid Sys.sigkill; log #warn "Worker PID %d killed with SIGKILL" pid 
      with 
      | Unix_error (ESRCH,_,_) -> ()
      | exn -> log #warn ~exn "Worker PID %d (SIGKILL)" pid) (reap l)
  in
  let rec reap_loop timeout l =
    match timeout, reap l with
    | _, [] -> log #info "All stopped"
    | Some 0, l -> log #info "Timeouted, killing with SIGKILL"; hard_kill l
    | _, l -> Nix.sleep 1.; reap_loop (Option.map pred timeout) l
  in
  log #info "Stopping %d workers" (List.length !l);
  alive := false;
  let l = List.map (fun (cin,cout,pid) ->
    close_in_noerr cin;
    close_out_noerr cout;
    begin try kill pid Sys.sigterm with exn -> log #warn ~exn "Worker PID %d lost (SIGTERM)" pid end;
    pid) !l
  in
  reap_loop wait l

let perform (l,execute,alive) e f =
    match !l with
    | [] -> Enum.iter (fun x -> f (execute x)) e (* no workers *)
    | _ ->
      let workers = ref 0 in
      List.iter (fun (i,o,p) ->
        match Enum.get e with
        | None -> ()
        | Some x -> incr workers; Marshal.to_channel o ( x : task) []; flush o) !l;
(*       Printf.printf "workers %u\n%!" !workers; *)
      while !workers > 0 && !alive do
        let fdl = List.map (fun (i,_,_) -> Unix.descr_of_in_channel i) !l in
(*         log #info "wait"; *)
        let (r,_,err) = Nix.restart (fun () -> Unix.select fdl [] fdl (-1.)) () in
        assert (err = []);
(*         log #info "wait done"; *)
        let channels = List.map (fun fd -> let (i,o,_) = List.find (fun (i,_,_) -> Unix.descr_of_in_channel i = fd) !l in i,o) r in
(*         log #info "channels done"; *)
        let answers = List.filter_map (fun (r,w) ->
          let task = Enum.get e in
          try 
(*             log #info "read"; *)
            match try Some (Marshal.from_channel r : result) with End_of_file -> None with
            | None ->
              log #warn "child gone, what now?";
              decr workers;
              let fd = Unix.descr_of_in_channel r in
              (* close and forget pipes of a dead child, do not reap zombie so that premature exit is visible in process list *)
              l := List.filter (fun (i,o,_) -> 
                if Unix.descr_of_in_channel i = fd
                then begin close_in_noerr i; close_out_noerr o; false end
                else true) !l;
              None
            | Some answer ->
(*             log #info "read done"; *)
            begin match task with
            | None -> decr workers
            | Some x ->
(*               log #info "write"; *)
              Marshal.to_channel w (x : task) []; flush w;
(*               log #info "write done"; *)
            end;
            Some answer
          with 
          | exn -> log #warn ~exn "perform"; decr workers; None)
        channels 
        in
        List.iter f answers;
      done

end

let invoke (f : 'a -> 'b) x : unit -> 'b =
  let input, output = Unix.pipe() in
  match Unix.fork() with
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
  match Unix.fork () with
  | 0 -> f x
  | -1 -> log #warn "failed to fork"
  | _ -> launch_forks f xs
