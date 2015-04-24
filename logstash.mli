(** Export counters registered with {!Var} as logstash events *)

val get : unit -> [> `Assoc of (string * [>`Floatlit of string | `Int of int | `String of string ]) list ] list
