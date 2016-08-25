
module C = Lwt_condition

type 'a t = { cond : 'a C.t; mutable waiter : 'a Lwt.t }

let create () =
  let cond = C.create () in
  { cond; waiter = C.wait cond }

let signal { cond; _ } x = C.signal cond x

let wait fl =
  let%lwt r = fl.waiter in
  fl.waiter <- C.wait fl.cond;
  Lwt.return r
