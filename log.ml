(**
  Global ready-to-use logger

  TODO interface to manage State
*)

(**
{2 Example usage}

Create logging facility (messages origin)
{[let http = Log.facility "http"]}

Log from http subsystem at debug level
{[Log.debug http "received %u bytes"]}

Create and use object for http logging
{[let log = Log.from "http" (* new Log.logger http *);;
log#info "sent %u bytes" 1024
log#warn ~exn "failed here"
]}

Output only messages of warning level or higher for the http facility
{[http#allow `Warn]}
or
{[Logger.set_filter http `Warn]}
or
{[Log.set_filter ~name:"http" `Warn]}

Output only messages of warning level or higher for all facilities
{[Log.set_filter `Warn]}

{2 API}
*)

open Printf
open ExtLib
open Prelude

(** Global logger state *)
module State = struct

  let process_name = ref @@ String.uppercase @@ Pid.name @@ Pid.self ()
  let all = Hashtbl.create 10
  let default_level = ref (`Info : Logger.level)

  let facility name =
    try
      Hashtbl.find all name
    with
      Not_found ->
        let x = { Logger.name = name; show = Logger.int_level !default_level } in
        Hashtbl.add all name x;
        x

  let set_filter ?name level =
    match name with
    | None -> default_level := level; Hashtbl.iter (fun _ x -> Logger.set_filter x level) all
    | Some name -> Logger.set_filter (facility name) level

  let set_process_name name =
    process_name := String.uppercase name

  let read_env_config ?(process_name=(!process_name)) () =
    let process_name = String.uppercase process_name in
    let facilities =
      try
        let env = Sys.getenv (process_name ^ "_LOG") in
        Stre.nsplitc env ','
      with Not_found ->
        []
    in
    List.iter begin fun facility ->
      try
        let name, level = Stre.splitc facility '=' in
        let level = Logger.level level in
        set_filter ~name level
      with
      | Not_found | Failure _ -> ()

    end facilities

  let output_ch ch =
    fun str -> try output_string ch str; flush ch with _ -> () (* logging never fails, most probably ENOSPC *)

  let format_simple level facil msg =
    let pid = Unix.getpid () in
    let tid = U.gettid () in
    let oid = Thread.id (Thread.self ()) in
    let pinfo = if pid = tid then sprintf "%5u:%u" pid oid else sprintf "%5u:%u:%u" pid oid tid in
    sprintf "[%s] %s [%s:%s] %s\n"
      (Time.to_string ~gmt:false ~ms:true (Unix.gettimeofday ()))
      pinfo
      facil.Logger.name
      (Logger.string_level level)
      msg

  let log_ch = stderr
  let () = assert (Unix.descr_of_out_channel stderr = Unix.stderr)
  let base_name = ref ""
  let need_rotation = ref (fun _ -> false)

  let hook = ref (fun _ _ _ -> ())

  module Put = Logger.PutSimple(
  struct
    let format = format_simple
    let output = fun level facil s -> let () = !hook level facil s in output_ch log_ch s
  end)

  module M = Logger.Make(Put)

  let self = "lib"

  (*
    we open the new fd, then dup it to stderr and close afterwards
    so we are always logging to stderr
  *)
  let reopen_log_ch ?(self_call=false) file =
    try
      if self_call = false then base_name := file;
      let ch = Files.open_out_append_text file in
      Std.finally
        (fun () -> close_out_noerr ch)
        (fun () -> Unix.dup2 (Unix.descr_of_out_channel ch) Unix.stderr)
        ()
    with
      e -> M.warn (facility self) "reopen_log_ch(%s) failed : %s" file (Printexc.to_string e)

  let find_possible_rotation () =
    let i = ref 0 in
    while Sys.file_exists (sprintf "%s.%d" !base_name !i) do incr i done;
    !i

  let rollback () =
    for i = 2 to 10 do
      try Unix.rename (sprintf "%s.%d" !base_name i) (sprintf "%s.%d" !base_name (i - 1)) with _ -> ()
    done;
    ()

  let do_rotation () =
    if !base_name <> "" then
    begin
      let i = find_possible_rotation () in
      if i > 10 then rollback ();
      let i = min i 10 in
      Sys.rename !base_name (sprintf "%s.%d" !base_name i);
      reopen_log_ch ~self_call:true !base_name
    end

  let check_rotation () =
    if !base_name <> "" then
    begin
      let stats = Unix.fstat (Unix.descr_of_out_channel log_ch) in
      (!need_rotation stats) && (stats.Unix.st_kind = Unix.S_REG)
    end else false (* no rotation with empty basename*)

  let rotation_i = ref 0

  let rotate () = incr rotation_i; if !rotation_i > 1_000 then begin rotation_i:=0; if check_rotation () then do_rotation () end

  let set_rotation f = need_rotation := f

end

include State.M

let facility = State.facility
let set_filter = State.set_filter
let set_process_name = State.set_process_name
let read_env_config = State.read_env_config

(**
  param [lines]: whether to split multiline message as separate log lines (default [true])

  param [backtrace]: whether to show backtrace (default is [true] if [exn] is given and backtrace recording is enabled)

  param [saved_backtrace]: supply backtrace to show instead of using [Printexc.get_backtrace]
*)
type 'a pr = ?exn:exn -> ?lines:bool -> ?backtrace:bool -> ?saved_backtrace:string list -> ('a, unit, string, unit) format4 -> 'a

class logger facil =
let make_s output_line =
  let output = function
  | true ->
      fun facil s ->
        if String.contains s '\n' then
          List.iter (output_line facil) @@ String.nsplit s "\n"
        else
          output_line facil s
  | false -> output_line
  in
  let print_bt lines exn bt s =
    output lines facil (s ^ " : exn " ^ Exn.str exn ^ (if bt = [] then " (no backtrace)" else ""));
    List.iter (fun line -> output_line facil ("    " ^ line)) bt
  in
  fun ?exn ?(lines=true) ?backtrace ?saved_backtrace s ->
    try
      State.rotate ();
      match exn with
      | None -> output lines facil s
      | Some exn ->
      match saved_backtrace with
      | Some bt -> print_bt lines exn bt s
      | None ->
      match Option.default (Printexc.backtrace_status ()) backtrace with
      | true -> print_bt lines exn (Exn.get_backtrace ()) s
      | false -> output lines facil (s ^ " : exn " ^ Exn.str exn)
    with exn ->
      output_line facil (sprintf "LOG FAILED : %S with message %S" (Exn.str exn) s)
in
let make output ?exn ?lines ?backtrace ?saved_backtrace fmt =
  ksprintf (fun s -> output ?exn ?lines ?backtrace ?saved_backtrace s) fmt
in
let debug_s = make_s debug_s in
let warn_s = make_s warn_s in
let info_s = make_s info_s in
let error_s = make_s error_s in
let put_s level = make_s (put_s level) in
object
method debug_s = debug_s
method warn_s = warn_s
method info_s = info_s
method error_s = error_s
method put_s = put_s

(* expecting direct inlining to be faster but it is not o_O
method debug : 'a. 'a pr =
  fun ?exn ?lines ?backtrace ?saved_backtrace fmt ->
  ksprintf (fun s -> debug_s ?exn ?lines ?backtrace ?saved_backtrace s) fmt
*)
method debug : 'a. 'a pr = make debug_s
method warn : 'a. 'a pr = make warn_s
method info : 'a. 'a pr = make info_s
method error : 'a. 'a pr = make error_s
method put : 'a. Logger.level -> 'a pr = fun level -> make (put_s level)

method allow (level:Logger.level) = Logger.set_filter facil level
method level : Logger.level = Logger.get_level facil
method name = facil.Logger.name
method facility : Logger.facil = facil
end

let from name = new logger (facility name)

(** internal logging facility *)
let self = from State.self

(** general logging facility *)
let main = from "main"

(** reopen log file *)
let reopen = function
| None -> ()
| Some name -> State.reopen_log_ch name

(** set log rotation **)
type rotation =
| No_rotation
| Days_rotation of int
| Size_rotation of int
| OnceAday_rotation

let log_start = ref (Time.now())
let cur_size = ref 0

let set_rotation = function
| No_rotation -> ()
| Days_rotation d -> State.set_rotation (fun _ -> let cur_time = Time.now() in if cur_time -. !log_start > (float d) *. 60. *. 60. then (log_start := Time.now();true) else false)
| Size_rotation s -> State.set_rotation (fun stats -> stats.Unix.st_size > s * 1024 * 1024)
| OnceAday_rotation -> State.set_rotation (fun _ -> let get_day s = (Unix.gmtime s).Unix.tm_yday in if get_day (Time.now()) <> get_day (!log_start) then (log_start := Time.now(); true) else false)
