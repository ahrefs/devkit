let log = Log.self

type 'a t = [ `Exn of exn | `None | `Ok of 'a ] ref * Thread.t
let detach f x =
  let result = ref `None in
  result, Thread.create (fun () -> result := Exn.map f x) ()
let join (result,thread) = Thread.join thread; match !result with `None -> assert false | (`Ok _ | `Exn _ as x) -> x
let join_exn t = match join t with `Ok x -> x | `Exn exn -> raise exn
let map f a = Array.map join_exn @@ Array.map (detach f) a
let mapn ?(n=8) f l =
  assert (n > 0);
  Action.distribute n l |> map (List.map @@ Exn.map f) |> Action.undistribute

let locked mutex f = Mutex.lock mutex; Std.finally (fun () -> Mutex.unlock mutex) f ()

module LockMutex = struct
  type t = Mutex.t
  let create = Mutex.create
  let locked = locked
end

module Async_fin = struct

  open Async
  module U = ExtUnix.All

  type t = { q : (unit -> unit) Mtq.t; evfd : Unix.file_descr; }

  let is_available () = ExtUnix.Config.have `EVENTFD

  let setup events =
    let fin = { q = Mtq.create (); evfd = U.eventfd 0; } in
    let rec loop () =
      match Mtq.try_get fin.q with
      | None -> ()
      | Some f -> begin try f () with exn -> log #warn ~exn "fin loop" end; loop ()
    in
    let reset fd =
      try
        ignore (U.eventfd_read fd)
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _) -> ()
      | exn -> log #warn ~exn "fin reset"; ()
    in
    setup_simple_event events fin.evfd [Ev.READ] begin fun _ fd _ -> reset fd; loop () end;
    fin

  let shutdown { q; evfd } = Mtq.clear q; Unix.close evfd

  let callback fin f =
    Mtq.put fin.q f;
    U.eventfd_write fin.evfd 1L

end

let log_create ?name f x = Thread.create (fun () -> Action.log ?name f x) ()

let run_periodic ~delay ?(now=false) f =
  let (_:Thread.t) = Thread.create begin fun () ->
    if not now then Nix.sleep delay;
    while try f () with exn -> Log.self #warn ~exn "ExtThread.run_periodic"; true do
      Nix.sleep delay
    done
  end ()
  in
  ()

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

module Workers(T:WorkerT) =
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
  for _ = 1 to n do
    ignore (Thread.create (fun () -> worker qi f qo) ())
  done;
  qi,qo,n

let perform (qi,qo,n) ?autoexit:_ e f =
  let active = ref 0 in
  for _ = 1 to n do
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

let atomic_incr = incr
let atomic_decr = decr
let atomic_get x = !x

module Pool = struct

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
      let (_:Thread.t) = log_create worker i in ()
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
