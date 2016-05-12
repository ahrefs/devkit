
open Printf

type level = [`Debug | `Info | `Warn | `Error]
type facil = { name : string; mutable show : int; }
let int_level = function
  | `Debug -> 0
  | `Info -> 1
  | `Warn -> 2
  | `Error -> 3
let set_filter facil level = facil.show <- int_level level
let get_level facil = match facil.show with
  | 0 -> `Debug
  | 1 -> `Info
  | 2 -> `Warn
  | _ -> `Error (* ! *)
let allowed facil level = int_level level >= facil.show

let string_level = function
  | `Debug -> "debug"
  | `Info -> "info"
  | `Warn -> "warn"
  | `Error -> "error"

let level = function
  | "info" -> `Info
  | "debug" -> `Debug
  | "warn" -> `Warn
  | "error" -> `Error
  | s -> Exn.fail "unrecognized level %s" s

module type Target =
sig
  val format : level -> facil -> string -> string
  val output : level -> facil -> string -> unit
end

module type Put = sig
val put : level -> facil -> string -> unit
end

module PutSimple(T : Target) : Put =
struct

  let put level facil str =
    if allowed facil level then
      T.output level facil (T.format level facil str)

end

module PutLimited(T : Target) : Put =
struct

  let last = ref (`Debug,"")
  let n = ref 0

  (** FIXME not thread safe *)
  let put level facil str =
    match allowed facil level with
    | false -> ()
    | true ->
      let this = (level,str) in
      if !last = this then
        incr n
      else
      begin
        if !n <> 0 then
        begin
         T.output level facil (sprintf
          "last message repeated %u times, suppressed\n" !n);
          n := 0
        end;
        last := this;
        T.output level facil (T.format level facil str);
      end

end

module Make(T : Put) = struct

  let debug_s = T.put `Debug
  let info_s = T.put `Info
  let warn_s = T.put `Warn
  let error_s = T.put `Error
  let put_s = T.put

  let debug f fmt = ksprintf (debug_s f) fmt
  let info f fmt = ksprintf (info_s f) fmt
  let warn f fmt = ksprintf (warn_s f) fmt
  let error f fmt = ksprintf (error_s f) fmt

end

