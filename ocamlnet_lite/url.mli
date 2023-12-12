(** Encoding/Decoding within URLs:
 *
 * The following two functions perform the '%'-substitution for
 * characters that may otherwise be interpreted as metacharacters.
 *
 * According to: RFC 1738, RFC 1630
 *
 * Option [plus]: This option has been added because there are some
 * implementations that do not map ' ' to '+', for example Javascript's
 * [escape] function. The default is [true] because this is the RFC-
 * compliant definition.
 *)

(** There are no tstring and polymorphic versions of the encode and
  decode functions, as URLs are comparatively short, and it is
  considered as acceptable for the user to convert types as needed,
  even if strings need to be copied for that.
*)

val decode : ?plus:bool -> ?pos:int -> ?len:int -> string -> string
(** Option [plus]: Whether '+' is converted to space. The default
  * is true. If false, '+' is returned as it is.
  *
  * The optional arguments [pos] and [len] may restrict the string
  * to process to this substring.
  *)

val encode : ?plus:bool -> string -> string
(** Option [plus]: Whether spaces are converted to '+'. The default
  * is true. If false, spaces are converted to "%20", and
  * only %xx sequences are produced.
  *)

val dest_url_encoded_parameters : string -> (string * string) list
(** The argument is the URL-encoded parameter string. The result is
  * the corresponding list of (name,value) pairs.
  * Note: Whitespace within the parameter string is ignored.
  * If there is a format error, the function fails.
  *)
