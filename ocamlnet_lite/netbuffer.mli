(** A Netbuffer.t is a buffer that can grow and shrink dynamically. *)

type t

val create : int -> t
    (** Creates a netbuffer which allocates initially this number of bytes. 
     * The logical length is zero.
     *)

val to_tstring_poly : t -> 's Netstring_tstring.tstring_kind -> 's
    (** Return the buffer in the format as selected by the arg *)

(** {2 Appending strings} *)

val add_string : t -> string -> unit
    (** [add_string nb s]: Adds a copy of the string [s] to the logical end of
     * the netbuffer [nb]. If necessary, [nb] grows.
     *)

