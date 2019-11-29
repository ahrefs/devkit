(** Manipulating network addresses *)

type ipv4
type ipv4_cidr

exception Parse_ipv4 of string

val ipv4_null : ipv4
val bytes_of_ipv4 : ipv4 -> int * int * int * int
val string_of_ipv4 : ipv4 -> string

(** @raise Parse_ipv4 if input is not an IP *)
val ipv4_of_string_exn : string -> ipv4

(** @return ip 0.0.0.0 when input is not an IP *)
val ipv4_of_string_null : string -> ipv4

val ipv4_of_int32 : int32 -> ipv4
val int32_of_ipv4 : ipv4 -> int32
val is_ipv4_slow : string -> bool
val is_ipv4 : string -> bool
val ipv4_of_int : int -> ipv4
val int_of_ipv4 : ipv4 -> int
val class_c : ipv4 -> ipv4
val ipv4_to_yojson : ipv4 -> Yojson.Safe.t
val ipv4_of_yojson : Yojson.Safe.t -> (ipv4, string) result

module IPv4 : sig
type t = ipv4
val equal : t -> t -> bool
val compare : t -> t -> int
val null : t
val to_bytes : t -> int * int * int * int
val to_string : t -> string
val of_string_exn : string -> t
val of_string_null : string -> t
val of_int32 : int32 -> t
val to_int32 : t -> int32
val of_int : int -> t
val to_int : t -> int
val class_c : t -> t
end

(** accepts addr/n notation or single ip *)
val cidr_of_string_exn : string -> ipv4_cidr
val range_of_cidr : ipv4_cidr -> ipv4 * ipv4
val prefix_of_cidr : ipv4_cidr -> ipv4
val ipv4_matches : ipv4 -> ipv4_cidr -> bool
val is_ipv4_special : ipv4 -> bool
val special_cidr : ipv4_cidr list

(** @return ip address of this machine on private network, with 127.0.0.1 as a fallback *)
val private_network_ip : unit -> Unix.inet_addr

(** @return interfaces and associated ip addresses of this machine on public network *)
val public_network_ips : unit -> (string * Unix.inet_addr) list

(** @return interfaces and associated ip addresses of this machine on private network *)
val private_network_ips : unit -> (string * Unix.inet_addr) list
