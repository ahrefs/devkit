
open Printf

type level = | Debug | Info | Warn | Error
type facil = string

let level_string = function
  | Debug -> "debug"
  | Info -> "info"
  | Warn -> "warn"
  | Error -> "error"

module type Target =
sig
  val filter : level -> facil -> string -> bool
  val format : level -> facil -> string -> string
  val output : string -> unit
end

module Simple(T : Target) =
struct

  let put level str =
    let facil = "" in
    if T.filter level facil str then 
      T.output (T.format level facil str)

  let logs_debug = put Debug
  let logs_info = put Info
  let logs_warn = put Warn
  let logs_error = put Error

  let log_debug fmt = ksprintf logs_debug fmt
  let log_info fmt = ksprintf logs_info fmt
  let log_warn fmt = ksprintf logs_warn fmt
  let log_error fmt = ksprintf logs_error fmt

end

let output_ch ch = 
  fun str -> output_string ch str; flush ch

let format_simple l _ s =
  sprintf "[%s] : %06u:%04u : [%5s] %s\n" (Time.gmt_string_ms (Unix.gettimeofday ())) (Unix.getpid ()) (Thread.id (Thread.self ())) (level_string l) s

let filter_none _ _ _ = true

