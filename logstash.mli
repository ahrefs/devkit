(** Export counters registered with {!Var} as logstash events *)

val get : unit -> [> `Assoc of (string * [> `Float of float | `Int of int | `String of string ]) list ] list
