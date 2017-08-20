
(* ExtThread.locked, duplicated to break internal circular dependency in ExtThread *)
let locked mutex f = Mutex.lock mutex; Std.finally (fun () -> Mutex.unlock mutex) f ()

type 'a t = { mutex : Mutex.t; cond : Condition.t; q : 'a Queue.t; }

let create () = { mutex = Mutex.create (); cond = Condition.create (); q = Queue.create (); }

let put q v = locked q.mutex (fun () -> Queue.push v q.q; Condition.signal q.cond)

let get q = locked q.mutex (fun () ->
  while Queue.is_empty q.q do Condition.wait q.cond q.mutex done;
  Queue.pop q.q)

let peek q = locked q.mutex (fun () ->
  while Queue.is_empty q.q do Condition.wait q.cond q.mutex done;
  Queue.peek q.q)

let junk q = locked q.mutex (fun () ->
  let _ = Exn.catch Queue.pop q.q in ())

let try_get q = locked q.mutex (fun () -> Exn.catch Queue.pop q.q)

let length q = locked q.mutex (fun () -> Queue.length q.q)

let clear q = locked q.mutex (fun () -> Queue.clear q.q)

