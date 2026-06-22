
open Printf

type level = [`Debug | `Info | `Warn | `Error | `Critical | `Nothing]
type facil = { name : string; mutable show : int; }
let int_level = function
  | `Debug -> 0
  | `Info -> 1
  | `Warn -> 2
  | `Error -> 3
  | `Critical -> 4
  | `Nothing -> 100
let set_filter facil level = facil.show <- int_level level
let get_level facil = match facil.show with
  | 0 -> `Debug
  | 1 -> `Info
  | 2 -> `Warn
  | 3 -> `Error
  | x when x = 100 -> `Nothing
  | _ -> `Critical (* ! *)
let allowed facil level = level <> `Nothing && int_level level >= facil.show

let string_level = function
  | `Debug -> "debug"
  | `Info -> "info"
  | `Warn -> "warn"
  | `Error -> "error"
  | `Critical -> "critical"
  | `Nothing -> "nothing"

let level = function
  | "info" -> `Info
  | "debug" -> `Debug
  | "warn" -> `Warn
  | "error" -> `Error
  | "critical" -> `Critical
  | "nothing" -> `Nothing
  | s -> Exn.fail "unrecognized level %s" s

module Pairs = struct
  type pair = string*string
  type t = pair list
end

type target = {
  format : level -> facil -> Time.t -> Pairs.t -> string -> string;
  mutable output : level -> facil -> string -> unit;
}

(** A logger *)
type t = {
  put : level -> facil -> Time.t -> Pairs.t -> string -> unit
} [@@unboxed]

let put_simple (t:target) : t = {
  put = fun level facil ts pairs str ->
    if allowed facil level then
      t.output level facil (t.format level facil ts pairs str)
}

let put_limited (t:target) : t =
  let last = ref (`Debug,"") in
  let n = ref 0 in

  (* FIXME not thread safe *)
  let put level facil ts pairs str =
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
         t.output level facil (sprintf
          "last message repeated %u times, suppressed\n" !n);
          n := 0
        end;
        last := this;
        t.output level facil (t.format level facil ts pairs str);
      end
  in { put }
