(** Punycode & IDN *)

module type CONV =
sig
  val upoints : string -> int array
  val ustring : int array -> string
end

module Make(CONV : CONV) :
sig
  exception Bad_input
  exception Overflow

  (** {1 punycode conversion} *)

  val encode : string -> string
  val decode : string -> string

  (** {1 IDN conversion} *)

  val encode_domain : string -> string
  val decode_domain : string -> string
end

(*

module CONV_Netconversion =
struct
  let upoints = Netconversion.uarray_of_ustring `Enc_utf8
  let ustring = Netconversion.ustring_of_uarray `Enc_utf8
end

module CONV_Camomile =
struct
  open CamomileLibraryDefault
  let upoints s = Array.init (Camomile.UTF8.length s) (fun i -> Camomile.UChar.uint_code (Camomile.UTF8.get s i))
  let ustring a = Camomile.UTF8.init (Array.length a) (fun i -> Camomile.UChar.chr_of_uint a.(i))
end

*)
