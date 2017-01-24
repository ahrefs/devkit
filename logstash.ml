open Printf
open ExtLib
open Prelude

let log = Log.from "logstash"

let state = Hashtbl.create 100

let dynamic = Hashtbl.create 100

let escape k =
  if String.contains k ' ' then
    String.map (function ' ' -> '_' | c -> c) k
  else
    k

let zero = Var.(function Count _ -> Count 0 | Time _ -> Time 0. | Bytes _ -> Bytes 0)

module Dyn = struct
  open Var
  type t = (string * [`Floatlit of string | `Int of int | `String of string ]) list

  let show_a = Action.strl (fun (c,_) -> sprintf "%S:''" c)

  let make_family family =
    let f = List.unique ~cmp:(fun (a,_) (b,_) -> a = b) family in
      if List.length f <> List.length family then log #warn "duplicate attributes : %s" (show_a family);
      List.sort ~cmp:(fun (a,_) (b,_) -> String.compare a b) family

  let make ?(attrs=[]) name =
    match is_in_families name with
    | true -> Exn.fail "static class with this name alreasdy exists: %s" name
    | false -> make_family (("class", `String name)::attrs)

  let extend dyn attrs =
    match attrs with
    | [] -> dyn
    | attrs -> make_family dyn @ attrs

  let set dyn ?(attrs=[]) v =
    let family = extend dyn attrs in
    Hashtbl.replace dynamic family v

  let add dyn ?(attrs =[]) v =
    let family = extend dyn attrs in
    match Hashtbl.find_default dynamic family (zero v),v with
    | Count x, Count x' ->
      let n = x + x' in
      Hashtbl.replace dynamic family (Count n)
    | Bytes x, Bytes x' ->
      let n = x + x' in
      Hashtbl.replace dynamic family (Bytes n)
    | Time x, Time x' ->
      let n = x +. x' in
      Hashtbl.replace dynamic family (Time n)
    | Count _, Bytes _ | Count _, Time _
    | Bytes _, Count _ | Bytes _, Time _
    | Time _, Count _ | Time _, Bytes _ -> log #warn "mismatched value type for %s" (show_a family)

  let set_count dyn attrs v = set dyn ~attrs (Count v)
  let set_bytes dyn attrs v = set dyn ~attrs (Bytes v)
  let set_time dyn attrs v = set dyn ~attrs (Time v)
  let add_count dyn attrs v = add dyn ~attrs (Count v)
  let add_bytes dyn attrs v = add dyn ~attrs (Bytes v)
  let add_time dyn attrs v = add dyn ~attrs (Time v)
end


let common_fields () =
  [
    "timestamp_ms", `Int (int_of_float @@ Time.now () *. 1000.);
    "pid", `String (Pid.show_self ());
    "toolname", `String (Pid.self ()).Pid.name;
  ]

let get () =
  let open Var in
  let l = ref [] in
  Var.iter begin fun attr v ->
    let (previous,attr) =
      try Hashtbl.find state attr with
      | Not_found ->
        let a = List.map (fun (k, s) -> escape k, `String s) attr in
        let x = ref (zero v), a in
        Hashtbl.add state attr x; x
    in
    let this = (common_fields () @ attr
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
  dynamic |> Hashtbl.iter begin fun attr v ->
    let attr = List.map (fun (k, s) -> escape k, s) attr in
    let this = (common_fields () @ attr
      :  (string * [ `Floatlit of string | `Int of int | `String of string]) list
      :> (string * [>`Floatlit of string | `Int of int | `String of string]) list)
    in
    let add c = tuck l @@ `Assoc (c :: this) in
    match v with
    | Count x -> add ("count", `Int x)
    | Bytes x -> add ("bytes", `Int x)
    | Time x -> add ("seconds", `Floatlit (sprintf "%g" x))
  end;
  Hashtbl.clear dynamic;
  !l

let get_basename () =
  match !Daemon.logfile with
  | None ->
    log #warn "no logfile, disabling logstash stats too";
    None
  | Some logfile ->
    let f = try Filename.chop_extension logfile with _ -> logfile in
    log #info "will output logstash stats to %s.YYYYMMDD.json" f;
    Some f

let open_logstash_exn basename =
  let filename = sprintf "%s.%s.json" basename (Time.format_date8 @@ Unix.gmtime @@ Time.now ()) in
  try
    Files.open_out_append_text filename
  with exn ->
    Exn.fail ~exn "failed to open stats file %s" filename

module J = Yojson

let write_json out nr json =
  let bytes = J.to_string ~std:true json ^ "\n" in
  try
    (* try not to step on the other forks toes, page writes are atomic *)
    if (String.length bytes > 4096 - !nr) then (flush out; nr := 0);
    output_string out bytes; nr := !nr + String.length bytes
  with exn -> log #warn ~exn "failed to write event %S" bytes

let line_writer out =
  let nr = ref 0 in
  write_json out nr

let setup_ setup =
  match get_basename () with
  | None -> ()
  | Some stat_basename ->
    setup begin fun () ->
      match open_logstash_exn stat_basename with
      | exception exn -> log #warn ~exn "disabling output"
      | ch ->
        Control.bracket ch close_out_noerr begin fun ch ->
          let write = line_writer ch in
          get () |> List.iter write;
          flush ch
        end
    end

let default_period = Time.seconds 60
let setup ?(pause=default_period) events = setup_ (fun f -> Async.setup_periodic_timer_wait events pause f)
let setup_lwt ?(pause=default_period) () =
  setup_ (fun f ->
    let rec loop () =
      match Daemon.should_exit () with
      | true -> Lwt.return_unit
      | false -> Lwt.bind (Lwt_unix.sleep pause) (fun () -> f (); loop ())
    in
    Lwt.async loop
  )

let round_to_midnight timestamp =
  let ms = Time.to_ms timestamp in
  let diff = ms mod (Time.days 1 |> Time.to_ms) in
  timestamp -. (Time.msec diff)

let is_same_day timestamp =
  Time.now () -. Time.days 1 < timestamp

let null = object method event _j = () method write () = () method reload () = () method autoflush () = () end

let log ?autoflush ?name () =
  let name = match name with None -> get_basename () | Some _ -> name in
  match name with
  | None -> null
  | Some stat_basename ->
    match open_logstash_exn stat_basename with
    | exception exn -> log #warn ~exn "disabling output"; null
    | out ->
      object(self)
        val mutable timestamp = round_to_midnight @@ Time.now ()
        val mutable out = out
        val nr = ref 0
        val mutable activity = false

        method autoflush =
          let l = lazy (
            match autoflush with
            | None -> ()
            | Some delay ->
              let rec l () =
                activity <- false;
                let%lwt () = Lwt_unix.sleep delay in
                if !nr > 0 && not activity then (flush out; nr := 0);
                l ()
              in
              Lwt.async l)
          in
          fun () -> Lazy.force l

        method reload () =
          try
            log #info "rotate log";
            let new_out = open_logstash_exn stat_basename in
            let prev = out in
            out <- new_out;
            nr := 0;
            timestamp <- round_to_midnight @@ Time.now ();
            flush prev;
            close_out_noerr prev
          with exn -> log #warn ~exn "failed to rotate log"

        method private try_rotate () =
          match timestamp with
          | t when not @@ is_same_day t -> self#reload ()
          | _ -> ()

        method write () =
          self#try_rotate ();
          get () |> List.iter (write_json out nr)

        method event (j : (string * J.json) list) =
          self#autoflush ();
          self#try_rotate ();
          let json = `Assoc (common_fields () @ j) in
          write_json out nr json;
          activity <- true

      end

let logstash_err = Lazy.from_fun @@ log ~name:"log/errors"

let setup_error_log () =
  Signal.set_reload !!logstash_err#reload;
  let chain_hook = !Log.State.hook in
  Log.State.hook := begin fun level facil s ->
    if level = `Error then begin
      let pid = Pid.self () in
      !!logstash_err #event ["error", `String facil.Logger.name; "pid", `Int pid.id; "pid_name", `String pid.name; "host", `String pid.host; "message", `String s];
    end;
    chain_hook level facil s
  end
