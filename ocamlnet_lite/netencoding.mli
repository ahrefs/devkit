(* *********************************************************************)
(* HTMLization                                                         *)
(* *********************************************************************)

(* THREAD-SAFETY:
 * The Html functions are thread-safe.
 *)

module Url : sig
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
end

module Html : sig
  (** Encodes characters that need protection by converting them to
    * entity references. E.g. ["<"] is converted to ["&lt;"].
    * As the entities may be named, there is a dependency on the character
    * set. 
    *)

  val encode :
    in_enc:Netconversion.encoding ->
    ?out_enc:Netconversion.encoding ->
    (* default: `Enc_usascii *)
    ?prefer_name:bool ->
    (* default: true *)
    ?unsafe_chars:string ->
    (* default: unsafe_chars_html4 *)
    unit ->
    string ->
    string
  (** The input string that is encoded as [in_enc] is recoded to 
    * [out_enc], and the following characters are encoded as HTML
    * entity ([&name;] or [&#num;]):
    * - The ASCII characters contained in [unsafe_chars]
    * - The characters that cannot be represented in [out_enc]. By
    *   default ([out_enc=`Enc_usascii]), only ASCII characters can be
    *   represented, and thus all code points >= 128 are encoded as
    *   HTML entities. If you pass [out_enc=`Enc_utf8], all characters
    *   can be represented.
    *
    * For example, the string ["(a<b) & (c>d)"] is encoded as
    * ["(a&lt;b) &amp; (c&gt;d)"].
    *
    * It is required that [out_enc] is an ASCII-compatible encoding.
    *
    * The option [prefer_name] selects whether named entities (e.g. [&lt;])
    * or numeric entities (e.g. [&#60;]) are prefered.
    * 
    * The efficiency of the function can be improved when the same encoding
    * is applied to several strings. Create a specialized encoding function
    * by passing all arguments up to the unit argument, and apply this
    * function several times. For example:
    * {[
    *     let my_enc = encode ~in_enc:`Enc_utf8 () in
    *     let s1' = my_enc s1 in
    *     let s2' = my_enc s2 in ...
    * ]}
    *)

  type entity_set = [ `Html | `Xml | `Empty ]

  val decode :
    in_enc:Netconversion.encoding ->
    out_enc:Netconversion.encoding ->
    ?lookup:(string -> string) ->
    (* default: see below *)
    ?subst:(int -> string) ->
    (* default: see below *)
    ?entity_base:entity_set ->
    (* default: `Html *)
    unit ->
    string ->
    string
  (** The input string is recoded from [in_enc] to [out_enc], and HTML
       * entities ([&name;] or [&#num;]) are resolved. The input encoding 
       * [in_enc] must be ASCII-compatible.
       *
       * By default, the function knows all entities defined for HTML 4 (this
       * can be changed using [entity_base], see below). If other
       * entities occur, the function [lookup] is called and the name of
       * the entity is passed as input string to the function. It is
       * expected that [lookup] returns the value of the entity, and that this
       * value is already encoded as [out_enc].
       * By default, [lookup] raises a [Failure] exception.
       *
       * If a character cannot be represented in the output encoding,
       * the function [subst] is called. [subst] must return a substitute
       * string for the character.
       * By default, [subst] raises a [Failure] exception.
       *
       * The option [entity_base] determines which set of entities are
       * considered as the known entities that can be decoded without
       * help by the [lookup] function: [`Html] selects all entities defined
       * for HTML 4, [`Xml] selects only [&lt;], [&gt;], [&amp;], [&quot;],
       * and [&apos;],
       * and [`Empty] selects the empty set (i.e. [lookup] is always called).
       *)

end
