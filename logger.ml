
open Printf

type level = [`Debug | `Info | `Warn | `Error | `Nothing]
type facil = { name : string; mutable show : int; }
let int_level = function
  | `Debug -> 0
  | `Info -> 1
  | `Warn -> 2
  | `Error -> 3
  | `Nothing -> 100
let set_filter facil level = facil.show <- int_level level
let get_level facil = match facil.show with
  | 0 -> `Debug
  | 1 -> `Info
  | 2 -> `Warn
  | x when x = 100 -> `Nothing
  | _ -> `Error (* ! *)
let allowed facil level = level <> `Nothing && int_level level >= facil.show

let string_level = function
  | `Debug -> "debug"
  | `Info -> "info"
  | `Warn -> "warn"
  | `Error -> "error"
  | `Nothing -> "nothing"

let level = function
  | "info" -> `Info
  | "debug" -> `Debug
  | "warn" -> `Warn
  | "error" -> `Error
  | "nothing" -> `Nothing
  | s -> Exn.fail "unrecognized level %s" s

module Pairs = struct
  type pair = string*string
  type t = pair list
end

type target = {
  format : level -> facil -> Time.t -> Pairs.t -> string -> string;
  output : level -> facil -> string -> unit;
}

type put = {
  put : level -> facil -> Time.t -> Pairs.t -> string -> unit
} [@@unboxed]

let put_simple (t:target) : put = {
  put = fun level facil ts pairs str ->
    if allowed facil level then
      t.output level facil (t.format level facil ts pairs str)
}

let put_limited (t:target) : put =
  let last = ref (`Debug,"") in
  let n = ref 0 in

  (* FIXME not thread safe *)
  let put level facil ts pairs str =
    match allowed facil level with
    | false -> ()
    | true ->
      let this = (level,str) in
      if !last = this then
        n := !n + 1
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

(** A logger *)
type t = {
  debug_s: facil -> Time.t -> Pairs.t -> string -> unit;
  info_s: facil -> Time.t -> Pairs.t -> string -> unit;
  warn_s: facil -> Time.t -> Pairs.t -> string -> unit;
  error_s: facil -> Time.t -> Pairs.t -> string -> unit;
  put_s: level -> facil -> Time.t -> Pairs.t -> string -> unit;
  debug: 'a. facil -> Time.t -> Pairs.t -> ('a, unit, string, unit) format4 -> 'a;
  info: 'a. facil -> Time.t -> Pairs.t -> ('a, unit, string, unit) format4 -> 'a;
  warn: 'a. facil -> Time.t -> Pairs.t -> ('a, unit, string, unit) format4 -> 'a;
  error: 'a. facil -> Time.t -> Pairs.t -> ('a, unit, string, unit) format4 -> 'a;
}

let make (t:put) : t =
  let debug_s = t.put `Debug in
  let info_s = t.put `Info in
  let warn_s = t.put `Warn in
  let error_s = t.put `Error in
  let put_s = t.put in
  let debug f ts pairs fmt = ksprintf (debug_s f ts pairs) fmt in
  let info f ts pairs fmt = ksprintf (info_s f ts pairs) fmt in
  let warn f ts pairs fmt = ksprintf (warn_s f ts pairs) fmt in
  let error f ts pairs fmt = ksprintf (error_s f ts pairs) fmt in
  { debug_s; info_s; warn_s; error_s; put_s; debug; info; warn; error }
