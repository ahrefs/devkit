
let locked mutex f = Mutex.lock mutex; Std.finally (fun () -> Mutex.unlock mutex) f ()

module LockMutex = struct
  type t = Mutex.t
  let create = Mutex.create
  let locked = locked
end

module Async_fin = struct

  open Async
  open Async.Internal
  module U = ExtUnix.Specific

  type t = { q : (unit -> unit) Mtq.t; evfd : Unix.file_descr; }

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

  let callback fin f =
    Mtq.put fin.q f;
    U.eventfd_write fin.evfd 1L

end
