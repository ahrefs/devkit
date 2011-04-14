
open Control

type 'a t = { mutex : Mutex.t; cond : Condition.t; mutable v : 'a option; }

let create () = { mutex = Mutex.create (); cond = Condition.create (); v = None; }

let set t x = locked t.mutex (fun () -> t.v <- Some x; Condition.signal t.cond)
let clear t = locked t.mutex (fun () -> t.v <- None)

let rec wait t =
  match t.v with
  | None -> Condition.wait t.cond t.mutex; wait t
  | Some x -> x

let get t = locked t.mutex (fun () -> wait t)
let grab t = locked t.mutex (fun () -> let x = wait t in t.v <- None; x)

let try_get t = locked t.mutex (fun () -> t.v)
let try_grab t = locked t.mutex (fun () -> let x = t.v in t.v <- None; x)

