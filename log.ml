(** 
  Global ready-to-use logger 

  TODO interface to manage State
*)

module State = struct

  let log_ch = ref stderr
  let output = ref (Logger.output_ch stderr)

  module M = Logger.Simple(
  struct
    let filter = Logger.filter_none
    let format = Logger.format_simple
    let output = fun s -> !output s
  end)

  let reopen_log_ch file =
    try
      let ch = Files.open_out_append file in
      output := Logger.output_ch ch;
      if !log_ch <> stderr then close_out_noerr !log_ch;
      log_ch := ch
    with
      e -> M.log_warn "reopen_log_ch(%s) failed : %s" file (Printexc.to_string e)

end

open State

let debug = M.log_debug
let info = M.log_info
let warn = M.log_warn
let error = M.log_error

let debug_s = M.logs_debug
let info_s = M.logs_info
let warn_s = M.logs_warn
let error_s = M.logs_error

