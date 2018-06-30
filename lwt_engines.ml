open ExtLib

module U = ExtUnix.Specific
module Ev = Libevent

class poll =
let readmask = U.Poll.(pollin + pollerr + pollhup + pollpri + pollrdhup) in
let writemask = U.Poll.(pollout + pollerr + pollhup) in
let convert (fd,i,o) = fd, U.Poll.((if i then pollin else none) + (if o then pollout else none)) in
object
val mutable buffer = [||]
inherit Lwt_engine.poll_based
method poll fds timeout =
(*
  let show = Stre.list (fun (fd,i,o) -> sprintf "%d%s%s" (U.int_of_file_descr fd) (if i then "r" else "") (if o then "w" else "")) in
  log #info "lwt poll %f %s" timeout (show fds);
*)
  let nfds = List.length fds in
  if nfds <= Array.length buffer && nfds * 2 > Array.length buffer then
  begin
    List.iteri (fun i x -> buffer.(i) <- convert x) fds;
  end
  else
    buffer <- Array.of_list @@ List.map convert fds;

  let l = U.poll buffer ~n:nfds timeout |> List.map (fun (fd,f) -> fd, U.Poll.is_inter f readmask, U.Poll.is_inter f writemask) in
(*   log #info "lwt poll done %s" (show l); *)
  l
end

(** libevent-based engine for lwt *)
class libevent =
let once_block = Ev.[ONCE] in
let once_nonblock = Ev.[ONCE;NONBLOCK] in
object(self)
  inherit Lwt_engine.abstract

  val events_ = Ev.init ()
  val mutable pid = Unix.getpid ()
  method events =
    if Unix.getpid () <> pid then (pid <- Unix.getpid (); Ev.reinit events_);
    events_

  method private cleanup = Ev.free events_

  method iter block =
    try
      Ev.(loops self#events (if block then once_block else once_nonblock))
    with
      exn -> Exn.fail ~exn "Lwt_engines.libevent#iter"

  method private register_readable fd f =
    let ev = Ev.create () in
    Ev.set self#events ev fd [Ev.READ] ~persist:true (fun _ _ -> f ());
    Ev.add ev None;
    lazy (Ev.del ev)

  method private register_writable fd f =
    let ev = Ev.create () in
    Ev.set self#events ev fd [Ev.WRITE] ~persist:true (fun _ _ -> f ());
    Ev.add ev None;
    lazy (Ev.del ev)

  method private register_timer delay repeat f =
    let ev = Ev.create () in
    let stop = ref false in
    Ev.set_timer self#events ev ~persist:false begin fun () ->
      if not !stop then f ();
      if repeat && not !stop then Ev.add ev (Some delay);
    end;
    Ev.add ev (Some delay);
    lazy (stop := true; Ev.del ev)

end
