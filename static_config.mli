(** Static mapping of simple config format.

Format is simple key-value pairs. Key must start with a letter and may include letters, numbers and underscore.
Value can be arbitrary - there are several ways to represent them :

* if the value doesn't contain any spaces, just write out the value directly

  <key> = <value>

* if the value is single-line, wrap it with any symbol (<DELIMITER> can be a quote, a doublequote or
  any other character that doesn't occur in the value itself)

  <key> := <DELIMITER><value><DELIMITER>

* multi-line symbols are written verbatim prefixed with the number of lines occupied

  <key> : <N>
  <line 1>
  <line 2>
  [...]
  <line N>


Example usage:

  module CONF = struct
    open Static_config

    (* fresh new group *)
    let root = new_root ()
    let save = save root
    let load = load root

    (* values stored *)
    let last_id = int root "last_id" 0
    let last_key = string root "last_key" ""
  end

*)

exception Error of string

type 'a value = < get : 'a; set : 'a -> unit; dirty : bool; >
type group

val group : group -> string -> group

val new_root : unit -> group

val int : group -> string -> int -> int value
val long : group -> string -> int64 -> int64 value
val string : group -> string -> string -> string value
val float : group -> string -> float -> float value
val bool : group -> string -> bool -> bool value

val show : ?all:bool -> group -> string
val read : group -> string -> unit

val reset : group -> unit
val load : group -> string -> unit
val save : ?all:bool -> group -> string -> unit

class base : group -> string -> object method load : unit -> unit method save : unit -> unit end
