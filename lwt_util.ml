
let log = Log.from "lwt_util"

let with_count nr lwt = incr nr; (lwt)[%lwt.finally decr nr; Lwt.return_unit]

let timely period f =
  assert (period > 0.);
  let next = ref (Time.get () +. period) in
  (fun x -> if Time.get () > !next then (next := Time.get () +. period; f x) else Lwt.return_unit)

let timely_loop' ?(immediate=false) period f =
  let rec loop () =
    let%lwt () = try%lwt f () with exn -> log #error ~exn "timely_loop_lwt"; Lwt.return_unit in
    let%lwt () = Lwt_unix.sleep period in
    loop ()
  in
  let%lwt () = if immediate then Lwt.return_unit else Lwt_unix.sleep period in
  loop ()

(* run f every period seconds; run immediately if immediate is true; stop when wait thread terminates *)
let timely_loop ?immediate ?(wait=Daemon.wait_exit ()) period f = Lwt.pick [ wait; timely_loop' ?immediate period f; ]

(* cancel t1 when t2 terminates *)
let ensure_order t1 t2 = (t2) [%finally Lwt.wrap1 Lwt.cancel t1; ]

(* wait for t to terminate, suppress any exception, and call cleanup () afterwards *)
let suppress_exn name cleanup t =
  log #info "%s started" name;
  let%lwt () =
    try%lwt
      let%lwt () = t in
      log #info "%s done" name;
      Lwt.return_unit
    with exn ->
      log #error ~exn "%s" name;
      Lwt.return_unit
  in
  cleanup ()

let action name f x =
  log #info "action %s started" name;
  match%lwt f x with
  | exception exn -> log #error ~exn "action %s aborted" name; Lwt.fail exn
  | x -> log #info "action %s done" name; Lwt.return x

let action_do name f = action name f ()

let async f = Lwt.async Daemon.(fun () -> try%lwt unless_exit (f ()) with ShouldExit -> Lwt.return_unit)
