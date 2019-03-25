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

module J = Yojson.Safe

type json = [ `Float of float | `Int of int | `String of string ]

module Dyn = struct
  open Var

  type t = (string * json) list

  let show_a = Stre.list (fun (c,_) -> sprintf "%S:''" c)

  let make_family family =
    let f = List.unique ~cmp:(fun (a,_) (b,_) -> a = b) family in
      if List.length f <> List.length family then log #warn "duplicate attributes : %s" (show_a family);
      List.sort ~cmp:(fun (a,_) (b,_) -> String.compare a b) family

  let make ?(attrs=[]) name =
    match is_in_families name with
    | true -> Exn.fail "static class with this name already exists: %s" name
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


let timestamp_field () = ("timestamp_ms", `Int Time.(to_ms @@ now ()))
let common_fields () =
  [
    timestamp_field ();
    "pid", `String (Pid.show_self ());
    "from", `String (Pid.self ()).Pid.name;
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
    let this = (common_fields () @ attr : (string * json) list :> (string * [> json ]) list) in
    match v, !previous with
    | Count x, Count x' ->
      let delta = x - x' in
      if delta <> 0 then begin previous := v; tuck l @@ `Assoc (("count", `Int delta) :: this) end
    | Bytes x, Bytes x' ->
      let delta = x - x' in
      if delta <> 0 then begin previous := v; tuck l @@ `Assoc (("bytes", `Int delta) :: this) end
    | Time x, Time x' ->
      let delta = x -. x' in
      if delta > epsilon_float then begin previous := v; tuck l @@ `Assoc (("seconds", `Float delta) :: this) end
    | Count _, Bytes _ | Count _, Time _
    | Bytes _, Count _ | Bytes _, Time _
    | Time _, Count _ | Time _, Bytes _ -> () (* cannot happen *)
  end;
  dynamic |> Hashtbl.iter begin fun attr v ->
    let attr = List.map (fun (k, s) -> escape k, s) attr in
    let this = (common_fields () @ attr : (string * json) list :> (string * [> json ]) list) in
    let add c = tuck l @@ `Assoc (c :: this) in
    match v with
    | Count x -> add ("count", `Int x)
    | Bytes x -> add ("bytes", `Int x)
    | Time x -> add ("seconds", `Float x)
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

let write_json activity out nr json =
  let bytes = J.to_string ~std:true json ^ "\n" in
  try
    (* try not to step on the other forks toes, page writes are atomic *)
    if (String.length bytes > 4096 - !nr) then (activity := true; flush out; nr := 0);
    output_string out bytes; nr := !nr + String.length bytes
  with exn -> log #warn ~exn "failed to write event %S" bytes

let line_writer out =
  let nr = ref 0 in
  write_json (ref false) out nr

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
let setup ?(pause=default_period) events = setup_ (fun f -> at_exit f; Async.setup_periodic_timer_wait events pause f)
let setup_lwt ?(pause=default_period) () =
  setup_ (fun f ->
    at_exit f;
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

type logger = <
  event : (string * Yojson.Safe.t) list -> unit;
  write : unit -> unit;
  reload : unit -> unit;
  flush : unit -> unit;
>

let null = object method event _j = () method write () = () method reload () = () method flush () = () end

let log ?autoflush ?(verbose=false) ?(add_timestamp_only=false) ?name () =
  let add_fields = if add_timestamp_only then fun l -> timestamp_field () :: l else fun l -> common_fields () @ l in
  let name = match name with None -> get_basename () | Some _ -> name in
  match name with
  | None -> null
  | Some stat_basename ->
    match open_logstash_exn stat_basename with
    | exception exn -> log #warn ~exn "disabling output"; null
    | out ->
      let out = ref out in
      let nr = ref 0 in
      let activity = ref false in
      begin match autoflush with
      | None -> ()
      | Some delay ->
        let rec l () =
          match Daemon.should_exit () with
          | true -> Lwt.return_unit
          | false ->
          activity := false;
          let%lwt () = Lwt_unix.sleep delay in
          if !nr > 0 && not !activity then (flush !out; nr := 0);
          l ()
        in
        Lwt.async l
      end;
      object(self)
        val mutable timestamp = round_to_midnight @@ Time.now ()

        method reload () =
          try
            if verbose then log #info "rotate log";
            let new_out = open_logstash_exn stat_basename in
            let prev = !out in
            out := new_out;
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
          get () |> List.iter (write_json activity !out nr)

        method event j =
          self#try_rotate ();
          write_json activity !out nr (`Assoc (add_fields j))

        method flush () =
          if !nr > 0 && not !activity then (flush !out; nr := 0)

      end

let logstash_err = Lazy.from_fun @@ log ~name:"log/errors"

let setup_error_log () =
  Signal.set_reload !!logstash_err#reload;
  let chain_hook = !Log.State.hook in
  Log.State.hook := begin fun level facil s ->
    if level = `Error then
    begin
      !!logstash_err #event ["facility", `String facil.Logger.name; "message", `String s];
      !!logstash_err #flush ();
    end;
    chain_hook level facil s
  end

let lifetime ?(extra="") ~events ~version () =
  let text = sprintf "%s start version %s" (Pid.self_name ()) version in
  events#event [
    "event", `String "start";
    "text", `String text;
    "extra", `String extra;
    "cmdline", `String Nix.cmdline;
    "cwd", `String (Sys.getcwd ());
    "user", `String (try Unix.(getpwuid @@ geteuid ()).pw_name with _ -> "");
    "version", `String version;
  ];
  events#flush ();
  Signal.set_exit begin fun () ->
    events#event [
      "event", `String "signal.stop";
      "text", `String (sprintf "%s received stop signal" (Pid.self_name ()));
      "version", `String version;
    ];
    events#flush ();
  end;
  let pid = Unix.getpid () in
  at_exit begin fun () ->
    match pid = Unix.getpid () with
    | false -> () (* forked child *)
    | true ->
      events#event [
        "event", `String "exit";
        "text", `String (sprintf "%s exit" (Pid.self_name ()));
        "version", `String version;
      ];
      events#flush ();
  end
