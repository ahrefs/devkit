(* WARNING! THIS IS A COPY OF NETSYS_TYPES.MLI! *)

(** Types for all Netsys modules *)

(** {2 Bytes and characters} *)

(** Remember that up to OCaml-4.01 there was only the [string] type,
    and strings were mutable (although frequently used as if there were
    immutable). Since OCaml-4.02 there is the immutable [string] and
    the mutable [bytes] type.

    The general strategy for switching to the string/bytes scheme is
    to replace [string] everywhere with [bytes], and to provide
    additional functions taking strings as input or output where it
    makes sense. There are exceptions, though, e.g. when the string
    acts as a key in a data structure.

    The type name "string" also occurs in function names (e.g.
    "get_string") and in variant names (e.g. [String_case]). As we
    want to be backward compatible, we keep the old names for functions
    on [bytes], and mark them as deprecated. 
 *)

type tbuffer = [ `Bytes of Bytes.t | `String of Bytes.t ]
  (** A tagged buffer. Note that the [`String] case is deprecated, and only
      provided for backward compatibility.
   *)

type tstring = [ `Bytes of Bytes.t | `String of string ]
  (** A tagged string which is considered as immutable. See also the
      support module {!Netstring_tstring}.
   *)
