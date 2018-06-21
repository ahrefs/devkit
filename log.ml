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
or
{[Log.set_filter ~name:"http*" `Warn]} to set for all facilities starting with "http"

Output only messages of warning level or higher for all facilities
{[Log.set_filter `Warn]}

{2 API}
*)

open Printf
open ExtLib
open Prelude

(** Global logger state *)
module State = struct

  let all = Hashtbl.create 10
  let default_level = ref (`Info : Logger.level)

  let utc_timezone = ref false

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
    | Some name when String.ends_with name "*" ->
      let prefix = String.slice ~last:(-1) name in
      Hashtbl.iter (fun k x -> if String.starts_with k prefix then Logger.set_filter x level) all
    | Some name -> Logger.set_filter (facility name) level

  let set_loglevels s =
    Stre.nsplitc s ',' |> List.iter begin fun spec ->
      match Stre.nsplitc spec '=' with
      | name :: l :: [] -> set_filter ~name (Logger.level l)
      | l :: [] -> set_filter @@ Logger.level l
      | _ -> Exn.fail "loglevel not recognized, specify either <level> or <facil>=<level> or <prefix>*=<level>"
    end

  let read_env_config ?(env="DEVKIT_LOG") () =
    set_loglevels @@ try Sys.getenv env with Not_found -> ""

  let output_ch ch =
    fun str -> try output_string ch str; flush ch with _ -> () (* logging never fails, most probably ENOSPC *)

  let format_simple level facil msg =
    let pid = Unix.getpid () in
    let tid = U.gettid () in
    let pinfo = if pid = tid then sprintf "%5u:" pid else sprintf "%5u:%u" pid tid in
    sprintf "[%s] %s [%s:%s] %s\n"
      (Time.to_string ~gmt:!utc_timezone ~ms:true (Unix.gettimeofday ()))
      pinfo
      facil.Logger.name
      (Logger.string_level level)
      msg

  let log_ch = stderr
  let () = assert (Unix.descr_of_out_channel stderr = Unix.stderr)
  let base_name = ref ""

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

end

include State.M

let facility = State.facility
let set_filter = State.set_filter
let set_loglevels = State.set_loglevels
let set_utc () = State.utc_timezone := true

(** Update facilities configuration from the environment.

    By default, it reads the configuration in the environment variable [DEVKIT_LOG]
    which can be overwritten using the optional [process_name] parameter.

    The value of environment variable should match the following grammar: [(\[<facil|prefix*>=\]debug|info|warn|error\[,\])*]

    @raise Failure on invalid level values of wrong format
*)
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

let log_start = ref (Time.now())
let cur_size = ref 0
