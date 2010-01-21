(** *)

module Int = struct
type t = int
external to_int : t -> int = "%identity"
external of_int : int -> t = "%identity"
let of_string = int_of_string
let to_string = string_of_int
let add = (+)
let zero = 0
let mul = ( * )
let neg = (~-)
end

