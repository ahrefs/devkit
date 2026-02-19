type attribute_value =
  [ `Int of int
  | `String of string
  | `Bool of bool
  | `Float of float
  | `None
  ]

type attribute = string * attribute_value

open struct
  let conv_level : Logger.level -> Trace_core.Level.t = function
    | `Nothing -> assert false
    | `Info -> Info
    | `Error -> Error
    | `Warn -> Warning
    | `Debug -> Debug1
end

class type t = object
  method set_attrs : attribute list -> unit
  method set_attr : string -> attribute_value -> unit
  method record_exn : exn -> Printexc.raw_backtrace -> unit
  method log : 'a. ('a, unit, string, unit) format4 -> 'a
  method span : Trace_core.span
  method finish : unit
end

(** Doesn't do anything *)
let dummy : t =
  object
    method set_attrs _ = ()
    method set_attr _ _ = ()
    method span = Trace_core.Collector.dummy_span
    method log : 'a. ('a, unit, string, unit) format4 -> 'a = fun fmt -> Printf.ksprintf ignore fmt
    method record_exn _ _ = ()
    method finish = ()
  end

let set_attr (self : #t) k v = self#set_attr k v
let set_attrs (self : #t) kvs = self#set_attrs kvs

let string_of_attr_v (v : attribute_value) =
  match v with
  | `Int i -> string_of_int i
  | `String s when not (String.contains s ' ') -> s
  | `String s -> Printf.sprintf "%S" s
  | `Bool b -> string_of_bool b
  | `None -> ""
  | `Float f -> Printf.sprintf "%.6f" f

let mk_msg_enter name : string =
  Printf.sprintf "logev.enter: %s" name

let mk_msg_exit ~(attrs : attribute list) ~(exn : _ option) name : string =
  let buf = Buffer.create (max 32 (16+String.length name)) in
  Printf.bprintf buf "logev.exit: %s" name;
  if attrs <> [] then (
    Printf.bprintf buf " ";
    List.iteri
      (fun i (k, v) ->
        if i > 0 then Printf.bprintf buf " ";
        Printf.bprintf buf "%s=%s" k (string_of_attr_v v))
      attrs;
    Printf.bprintf buf "");

  Stdlib.Option.iter
    (fun (exn, bt) ->
      Printf.bprintf buf " [ERROR %s]\n%s" (Printexc.to_string exn) (Printexc.raw_backtrace_to_string bt))
    exn;

  Buffer.contents buf

module Enricher = struct
  type t = unit -> attribute list

  (* never change *)
  let common_static_attrs_ : attribute list =
    [
      "unix.pid", `Int (Unix.getpid ());
      "unix.cmdline", `String Nix.cmdline;
      "process.start", `String (Time.fast_to_string ~gmt:true !Log.log_start);
      "systemd.booted", `Bool Systemd.Daemon.booted;
    ]

  (* change slowly or rarely *)
  let common_dyn_attrs_ () : attribute list =
    (match !Daemon.pidfile with
      | None -> []
      | Some s -> [ "daemon.pidfile", `String s ])
    @ (match !Daemon.runas with
      | None -> []
      | Some s -> [ "daemon.runas.uid", `Int s.pw_uid; "daemon.runas.name", `String s.pw_name ])
    @ [ "daemon.managed", `Bool !Daemon.managed ]

  let cached ~(timeout_s : float) (e : t) : t =
    let last_updated = ref (Unix.gettimeofday ()) in
    let value = ref (e ()) in
    fun () ->
      let now = Unix.gettimeofday () in
      if now > !last_updated +. timeout_s then (
        last_updated := now;
        value := e ());
      !value

  (* some memory-related statistics that don't go stale immediately *)
  let memory : t = fun () ->
    let stats = Gc.stat() in
    let vm = Memory.get_vm_info () in
    [
      "gc.recent.major.collections", `Int stats.major_collections;
      "gc.recent.major.size", `Float (stats.major_words *. 8.);
      "gc.recent.compactions", `Int stats.compactions;
      "vm.recent.rss", `Int vm.rss;
      "vm.recent.vsize", `Int vm.vsize;
    ]

  let all_ : t list ref = ref [
    (fun () -> common_static_attrs_);
    cached ~timeout_s:300. common_dyn_attrs_;
    cached ~timeout_s:15. memory;
  ]
  let add f = all_ := f :: !all_
end

let create ~(level : Logger.level) ~(log : #Log.logger) ~(span : Trace_core.span) (name : string) : t =
  let t_start = Unix.gettimeofday () in
  let finished = ref false in
  let exn_r = ref None in
  let attrs_r = ref [] in

  (* enrich with contextual info *)
  List.iter
    (fun e ->
      let attrs = e () in
      Trace_core.add_data_to_span span attrs;
      attrs_r := List.rev_append attrs !attrs_r)
    !Enricher.all_;

  (* just print that we're entering *)
  let msg_enter = mk_msg_enter name in
  log#put level "%s" msg_enter;

  object
    method set_attr k v =
      attrs_r := (k, v) :: !attrs_r;
      Trace_core.add_data_to_span span [ k, v ]
    method set_attrs attrs =
      attrs_r := List.rev_append attrs !attrs_r;
      Trace_core.add_data_to_span span attrs
    method span = span
    method log : 'a. ('a, unit, string, unit) format4 -> 'a = fun fmt ->
      Printf.ksprintf (log#put_s level) fmt
    method record_exn exn bt =
      exn_r := Some (exn, bt);
      Trace_core.add_data_to_span span
        [
          "exception.message", `String (Printexc.to_string exn);
          "exception.stacktrace", `String (Printexc.raw_backtrace_to_string bt);
        ]
    method finish =
      (* idempotent *)
      if not !finished then (
        finished := true;
        Trace_core.exit_span span;

        let t_stop = Unix.gettimeofday () in
        attrs_r := [ "duration_s", `Float (t_stop -. t_start) ] @ !attrs_r;

        let msg = mk_msg_exit ~attrs:!attrs_r ~exn:!exn_r name in
        log#put level "%s" msg)
  end

let enter ?__FUNCTION__ ~__FILE__ ~__LINE__ ?(attrs = []) ~(log : #Log.logger) ?(level = `Debug) (name : string) :
  #t =
  match level with
  | `Nothing -> dummy
  | _ ->
    let span : Trace_core.span = Trace_core.enter_span ~level:(conv_level level) ~__FILE__ ~__LINE__ name in
    let logev : #t = create ~level ~span ~log name in
    set_attrs logev attrs;
    logev

let exit (self : #t) : unit = self#finish

type 'a enter_args =
  ?__FUNCTION__:string ->
  __FILE__:string ->
  __LINE__:int ->
  ?attrs:attribute list ->
  log:Log.logger ->
  'a

let enter_info ?__FUNCTION__ ~__FILE__ ~__LINE__ ?attrs ~log name =
  enter ?__FUNCTION__ ~__FILE__ ~__LINE__ ?attrs ~log ~level:`Info name
let enter_debug ?__FUNCTION__ ~__FILE__ ~__LINE__ ?attrs ~log name =
  enter ?__FUNCTION__ ~__FILE__ ~__LINE__ ?attrs ~log ~level:`Debug name
let enter_warn ?__FUNCTION__ ~__FILE__ ~__LINE__ ?attrs ~log name =
  enter ?__FUNCTION__ ~__FILE__ ~__LINE__ ?attrs ~log ~level:`Warn name
let enter_error ?__FUNCTION__ ~__FILE__ ~__LINE__ ?attrs ~log name =
  enter ?__FUNCTION__ ~__FILE__ ~__LINE__ ?attrs ~log ~level:`Error name

(* TODO: when updating ambient-context:

(** Key to access the current logev in a hmap or ambient-context *)
let k_ambient : t Ambient_context.key = Ambient_context.new_key ()

let get_current () : t option = Ambient_context.get k_ambient

TODO: then register a middleware for httpev that creates a logev and registers
it as ambient
*)
