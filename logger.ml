(** Logger primitives.

  Domain safety: [t.put], {!allowed}, {!get_level} and {!set_filter} are safe to call
  from any domain, and [target.output] may be swapped (with [Atomic.set]) from any domain.
  Reconfiguration is not atomic with respect to messages being logged concurrently:
  a message that passed the old filter may still be emitted, possibly via the new output.
  [target.output] itself is called from whichever domain logs, so it must be domain-safe. *)

type level = [`Debug | `Info | `Warn | `Error | `Critical | `Nothing]
type facil = { name : string; show : int Atomic.t; }
let int_level = function
  | `Debug -> 0
  | `Info -> 1
  | `Warn -> 2
  | `Error -> 3
  | `Critical -> 4
  | `Nothing -> 100
let set_filter facil level = Atomic.set facil.show (int_level level)
let get_level facil = match Atomic.get facil.show with
  | 0 -> `Debug
  | 1 -> `Info
  | 2 -> `Warn
  | 3 -> `Error
  | x when x = 100 -> `Nothing
  | _ -> `Critical (* ! *)
let allowed facil level = level <> `Nothing && int_level level >= Atomic.get facil.show

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
  output : (level -> facil -> string -> unit) Atomic.t;
}

(** A logger *)
type t = {
  put : level -> facil -> Time.t -> Pairs.t -> string -> unit;
  allowed : facil -> level -> bool;
}

let put_simple (t:target) : t = {
  allowed;
  put = fun level facil ts pairs str ->
    if allowed facil level then
      (Atomic.get t.output) level facil (t.format level facil ts pairs str)
}
