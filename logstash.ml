open Printf
open ExtLib
open Prelude

let log = Log.from "logstash"

let state = Hashtbl.create 10

let escape k =
  if String.contains k ' ' then
    String.map (function ' ' -> '_' | c -> c) k
  else
    k

let zero = Var.(function Count _ -> Count 0 | Time _ -> Time 0. | Bytes _ -> Bytes 0)

let get () =
  let common =
    [
      "timestamp_ms", `Int (int_of_float @@ Time.now () *. 1000.);
      "pid", `String (Pid.show_self ());
    ]
  in
  let l = ref [] in
  Var.iter begin fun attr v ->
    let (previous,attr) =
      try Hashtbl.find state attr with
      | Not_found -> let x = ref (zero v), List.map (fun (k, s) -> escape k, `String s) attr in Hashtbl.add state attr x; x
    in
    let this = (common @ attr
      :  (string * [ `Floatlit of string | `Int of int | `String of string]) list
      :> (string * [>`Floatlit of string | `Int of int | `String of string]) list)
    in
    match v, !previous with
    | Count x, Count x' ->
      let delta = x - x' in
      if delta <> 0 then begin previous := v; tuck l @@ `Assoc (("count", `Int delta) :: this) end
    | Bytes x, Bytes x' ->
      let delta = x - x' in
      if delta <> 0 then begin previous := v; tuck l @@ `Assoc (("bytes", `Int delta) :: this) end
    | Time x, Time x' ->
      let delta = x -. x' in
      if delta > epsilon_float then begin previous := v; tuck l @@ `Assoc (("seconds", `Floatlit (sprintf "%g" delta)) :: this) end
    | Count _, Bytes _ | Count _, Time _
    | Bytes _, Count _ | Bytes _, Time _
    | Time _, Count _ | Time _, Bytes _ -> () (* cannot happen *)
  end;
  !l

let setup_ setup =
  match !Daemon.logfile with
  | None -> log #warn "no logfile, disabling logstash stats too"
  | Some logfile ->
    let stat_basename = try Filename.chop_extension logfile with _ -> logfile in
    log #info "will output logstash stats to %s.YYYYMMDD.json" stat_basename;
    setup 60. begin fun () ->
      let filename = sprintf "%s.%s.json" stat_basename (Time.format_date8 @@ Unix.gmtime @@ Time.now ()) in
      match Files.open_out_append_text filename with
      | exception exn -> log #warn ~exn "failed to open stats file %s" filename
      | ch ->
        Control.bracket ch close_out_noerr begin fun ch ->
          let nr = ref 0 in
          get () |> List.iter begin fun v ->
            let s = Yojson.to_string ~std:true v ^ "\n" in
            (* try not to step on the other forks toes, page writes are atomic *)
            if !nr + String.length s > 4096 then (flush ch; nr := 0);
            output_string ch s;
            nr += String.length s
          end;
          flush ch;
        end
    end

let setup events = setup_ (fun pause f -> Async.setup_periodic_timer_wait events pause f)
let setup_lwt () =
  setup_ (fun pause f ->
    let rec loop () =
      match Daemon.should_exit () with
      | true -> Lwt.return_unit
      | false -> Lwt.bind (Lwt_unix.sleep pause) (fun () -> f (); loop ())
    in
    Lwt.async loop
  )
