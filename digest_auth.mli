
type t
val init : realm:string -> user:string -> password:string -> unit -> t
val check : t -> Httpev_common.request -> [`Ok | `Unauthorized of (string * string)]
