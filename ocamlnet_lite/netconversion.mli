(** Conversion between character encodings 
 *
 * {b Contents}
 * {ul
 *   {- {!Netconversion.preliminaries}
 *     {ul
 *       {- {!Netconversion.unicode}}
 *       {- {!Netconversion.subsets}}
 *       {- {!Netconversion.linking}}
 *       {- {!Netconversion.domain}}
 *       {- {!Netconversion.problems}}}}
 *   {- {!Netconversion.interface}
 *     {ul
 *       {- {!Netconversion.direct_conv}}
 *       {- {!Netconversion.cursors}
 *           {ul {- {!Netconversion.bom}}}}
 *       {- {!Netconversion.unicode_functions}}
 *     }
 *   }
 * }
 *)

(** {1:preliminaries Preliminaries}
 *
 * A {b character set} is a set of characters where every character is
 * identified by a {b code point}. An {b encoding} is a way of 
 * representing characters from a set in byte strings. For example,
 * the Unicode character set has more than 96000 characters, and
 * the code points have values from 0 to 0x10ffff (not all code points
 * are assigned yet). The UTF-8 encoding represents the code points
 * by sequences of 1 to 4 bytes. There are also encodings that 
 * represent code points from several sets, e.g EUC-JP covers four
 * sets.
 *
 * Encodings are enumerated by the type [encoding], and names follow
 * the convention [`Enc_*], e.g. [`Enc_utf8]. 
 * Character sets are enumerated by the type
 * [charset], and names follow the convention [`Set_*], e.g.
 * [`Set_unicode].
 *
 * This module deals mainly with encodings. It is important to know
 * that the same character set may have several encodings. For example,
 * the Unicode character set can be encoded as UTF-8 or UTF-16.
 * For the 8 bit character sets, however, there is usually only one
 * encoding, e.g [`Set_iso88591] is always encoded as [`Enc_iso88591].
 *
 * In a {b single-byte encoding} every code point is represented by
 * one byte. This is what many programmers are accustomed at, and
 * what the OCaml language specially supports: A [string] is
 * a sequence of [char]s, where [char] means an 8 bit quantity
 * interpreted as character. For example, the following piece of code allocates
 * a [string] of four [char]s, and assigns them individually:
 *
 * {[
 * let s = String.create 4 in
 * s.[0] <- 'G';
 * s.[1] <- 'e';
 * s.[2] <- 'r';
 * s.[3] <- 'd';
 * ]}
 * 
 * In a {b multi-byte encoding} there are code points that are represented
 * by several bytes. As we still represent such text as [string], the
 * problem arises that a single [char], actually a byte, often represents 
 * only a fraction of a full multi-byte character. There are two solutions:
 * - Give up the principle that text is represented by [string].
 *   This is, for example, the approach chosen by [Camomile], another OCaml
 *   library dealing with Unicode. Instead, text is represented as
 *   [int array]. This way, the algorithms processing the text can
 *   remain the same.
 * - Give up the principle that individual characters can be directly
 *   accessed in a text. This is the primary way chosen by Ocamlnet.
 *   This means that there is not any longer the possibility to read
 *   or write the [n]th character of a text. One can, however, still 
 *   compose texts by just concatenating the strings representing
 *   individual characters. Furthermore, it is possible to define
 *   a cursor for a text that moves sequentially along the text.
 *   The consequence is that programmers are restricted to sequential
 *   algorithms. Note that the majority of text processing falls into
 *   this class.
 *
 * The corresponding piece of code for Ocamlnet's Unicode implementation
 * is:
 * {[
 * let b = Buffer.create 80 in
 * Buffer.add b (ustring_of_uchar `Enc_utf8 71);  (* 71 = code point of 'G' *)
 * Buffer.add b (ustring_of_uchar `Enc_utf8 101); (* 101 = code point of 'e' *)
 * Buffer.add b (ustring_of_uchar `Enc_utf8 114); (* 114 = code point of 'r' *)
 * Buffer.add b (ustring_of_uchar `Enc_utf8 100); (* 100 = code point of 'd' *)
 * let s = Buffer.contents b
 * ]}
 *
 * It is important to always remember that a [char] is no longer 
 * a character but simply a byte. In many of the following explanations,
 * we strictly distinguish between {b byte positions} or {b byte counts},
 * and {b character positions} or {b character counts}.
 *
 * There a number of special effects that usually only occur in
 * multi-byte encodings:
 *
 * - Bad encodings: Not every byte sequence is legal. When scanning
 *   such text, the functions will raise the exception [Malformed_code]
 *   when they find illegal bytes.
 * - Unassigned code points: It may happen that a byte sequence is
 *   a correct representation for a code point, but that the code point
 *   is unassigned in the character set. When scanning, this is also
 *   covered by the exception [Malformed_code]. When converting from
 *   one encoding to another, it is also possible that the code point
 *   is only unassigned in the target character set. This case is
 *   usually handled by a substitution function [subst], and if no such
 *   function is defined, by the exception [Cannot_represent].
 * - Incomplete characters: The trailing bytes of a string may be the
 *   correct beginning of a byte sequence for a character, but not a
 *   complete sequence. Of course, if that string is the end of a
 *   text, this is just illegal, and also a case for [Malformed_code].
 *   However, when text is processed chunk by chunk, this phenomenon
 *   may happen legally for all chunks but the last. For this reason,
 *   some of the functions below handle this case specially.
 * - Byte order marks: Some encodings have both big and little endian
 *   variants. A byte order mark at the beginning of the text declares
 *   which variant is actually used. This byte order mark is a 
 *   declaration written like a character, but actually not a 
 *   character.
 *
 * There is a special class of encodings known as {b ASCII-compatible}.
 * They are important because there are lots of programs and protocols
 * that only interpret bytes from 0 to 127, and treat the bytes from
 * 128 to 255 as data. These programs can process texts as long as
 * the bytes from 0 to 127 are used as in ASCII. Fortunately, many
 * encodings are ASCII-compatible, including UTF-8.
 *
 * {2:unicode Unicode}
 *
 * [Netconversion] is centred around Unicode.
 * The conversion from one encoding to another works by finding the
 * Unicode code point of the character
 * to convert, and by representing the code point in the target encoding,
 * even if neither encodings have to do with Unicode.
 * Of course, this approach requires that all character sets handled
 * by [Netconversion] are subsets of Unicode.
 *
 * The supported range of Unicode code points: 0 to 0xd7ff, 0xe000 to 0xfffd,
 * 0x10000 to 0x10ffff. All these code points can be represented in 
 * UTF-8 and UTF-16. [Netconversion] does not know which of the code
 * points are assigned and which not, and because of this, it simply
 * allows all code points of the mentioned ranges (but for other character
 * sets, the necessary lookup tables exist).
 *
 * {b UTF-8:} The UTF-8 representation can have one to four bytes. Malformed 
 *   byte sequences are always rejected, even those that want to cheat the
 *   reader like "0xc0 0x80" for the code point 0. There is special support
 *   for the Java variant of UTF-8 ([`Enc_java]). [`Enc_utf8] strings must not
 *   have a byte order mark (it would be interpreted as "zero-width space"
 *   character). However, the Unicode standard allows byte order marks
 *   at the very beginning of texts; use [`Enc_utf8_opt_bom] in this case.
 *
 * {b UTF-16:} When reading from a string encoded as [`Enc_utf16], a byte
 *   order mark is expected at the beginning. The detected variant 
 *   ([`Enc_utf16_le] or [`Enc_utf16_be]) is usually returned by the parsing
 *   function. The byte order mark is not included into the output string. - 
 *   Some functions of this
 *   module cannot cope with [`Enc_utf16] (i.e. UTF-16 without endianess
 *   annotation), and will fail.
 *
 *   Once the endianess is determined, the code point 0xfeff is no longer
 *   interpreted as byte order mark, but as "zero-width non-breakable space".
 *
 *   Some code points are represented by pairs of 16 bit values, these
 *   are the so-called "surrogate pairs". They can only occur in UTF-16.
 *
 * {b UTF-32:} This is very much the same as for UTF-16. There is a little
 *   endian version [`Enc_utf32_le] and a big endian version [`Enc_utf32_be].
 *
 * {2:subsets Subsets of Unicode}
 *
 * The non-Unicode character sets are subsets of Unicode. Here, it may
 * happen that a Unicode code point does not have a corresponding 
 * code point. In this case, certain rules are applied to handle
 * this (see below). It is, however, ensured that every non-Unicode
 * code point has a corresponding Unicode code point. (In other words,
 * character sets cannot be supported for which this property does
 * not hold.)
 *
 * It is even possible to create further subsets artificially. The
 * encoding [`Enc_subset(e,def)] means to derive a new encoding from
 * the existing one [e], but to only accept the code points for which
 * the definition function [def] yields the value [true]. For example,
 * the encoding 
 * {[ `Enc_subset(`Enc_usascii, 
 *             fun i -> i <> 34 && i <> 38 && i <> 60 && i <> 62) ]}
 * is ASCII without the bracket angles, the quotation mark, and the
 * ampersand character, i.e. the subset of ASCII that can be included
 * in HTML text without escaping.
 *
 * If a code point is not defined by the encoding but found in a text, 
 * the reader will raise the exception [Malformed_code]. When text is
 * output, however, the [subst] function will be called for undefined code 
 * points (which raises [Cannot_represent] by default). The [subst]
 * function is an optional argument of many conversion functions that
 * allows it to insert a substitution text for undefined code points.
 * Note, however, that the substitution text is restricted to at most
 * 50 characters (because unlimited length would lead to difficult
 * problems we would like to avoid).
 *
 * {2:linking Linking this module}
 *
 * Many encodings require lookup tables. The following encodings
 * are built-in and always supported:
 *
 * - Unicode: [`Enc_utf8], [`Enc_java], [`Enc_utf16], [`Enc_utf16_le], 
     [`Enc_utf16_be], [`Enc_utf32], [`Enc_utf32_le], [`Enc_utf32_be]
 * - Other: [`Enc_usascii], [`Enc_iso88591], [`Enc_empty]
 *
 * The lookup tables for the other encodings are usually loaded at
 * runtime, but it is also possible to embed them in the generated
 * binary executable. See {!Netunidata} for details. The functions
 * [available_input_encodings] and [available_output_encodings] can
 * be invoked to find out which encodings can be loaded, or are available
 * otherwise.
 *
 * {2:domain Supported Encodings, Restrictions}
 *
 * I took the mappings from [www.unicode.org], and the standard names of
 * the character sets from IANA. Obviously, many character sets are missing
 * that can be supported; especially ISO646 character sets, and many EBCDIC 
 * code pages. Stateful encodings like generic ISO-2022 have been omitted
 * (stateless subsets of ISO-2022 like EUC can be supported, however;
 * currently we support EUC-JP and EUC-KR).
 *
 * Because of the copyright statement from Unicode, I cannot put the
 * source tables that describe the mappings into the distribution. They
 * are publicly available from [www.unicode.org].
 *
 * {2:problems Known Problems}
 *
 * - The following charsets do not have a bijective mapping to Unicode:
 *   adobe_standard_encoding, adobe_symbol_encoding, 
 *   adobe_zapf_dingbats_encoding, cp1002 (0xFEBE). The current implementation
 *   simply removes one of the conflicting code point pairs - this might
 *   not what you want.
 * - Japanese encodings: 
 *   JIS X 0208: The character 1/32 is mapped to 0xFF3C, and not
 *   to 0x005C.
 *)

(** {1:interface Interface}
 *
 * {b Naming conventions:}
 *
 * As it is possible to refer to substrings by either giving a byte
 * offset or by counting whole characters, these naming conventions
 * are helpful:
 *
 * - Labels called [range_pos] and [range_len] refer to byte positions of
 *   characters, or substrings
 * - Labels called [count] refer to positions given as the number of characters
 *   relative to an origin
 *
 * Furthermore:
 * 
 * - A [uchar] is a single Unicode code point represented as int
 * - A [ustring] is a string of encoded characters
 * - A [uarray] is an [array of int] representing a string
 *)

type encoding =
  [ `Enc_utf8 (* UTF-8 *)
  | (* Encoding subset: *)
    `Enc_subset of encoding * (int -> bool) ]
(** The polymorphic variant enumerating the supported encodings. We have:
 * - [`Enc_utf8]: UTF-8
 * - [`Enc_subset(e,def)]: The subset of [e] by applying the definition 
 *   function [def]
 *)

(**********************************************************************)
(* String functions                                                   *)
(**********************************************************************)

(** {2:unicode_functions Unicode String Functions} *)

val uarray_of_ustring :
  encoding -> ?range_pos:int -> ?range_len:int -> string -> int array
(** Returns the characters of the string as array of Unicode code points.
   * 
   * @param range_pos The byte position of the substring to extract
   *   (default: 0)
   * @param range_len The byte length of the substring to extract
   *   (default: byte length of the input string minus [range_pos])
   *)

val ustring_of_uarray :
  ?subst:(int -> string) ->
  encoding ->
  ?pos:int ->
  ?len:int ->
  int array ->
  string
(** Returns the array of Unicode code points as encoded string.
   * 
   * @param pos Selects a subarray: [pos] is the first array position
   *   to encode (default: 0)
   * @param len Selects a subarray: [len] is the length of the subarray
   *   to encode (default: array length minus [pos])
   * @param subst This function is called when a code point cannot be represented
   *   in the chosen character encoding. It must returns the (already encoded)
   *   string to substitute for this code point. By default 
   *   (if ~subst is not passed), the exception [Cannot_represent] 
   *   will be raised in this case.
   *)

val convert_poly :
  in_ops:'s1 Netstring_tstring.tstring_ops ->
  out_kind:'s2 Netstring_tstring.tstring_kind ->
  ?subst:(int -> string) ->
  in_enc:encoding ->
  out_enc:encoding ->
  ?range_pos:int ->
  ?range_len:int ->
  's1 ->
  's2

val convert :
  ?subst:(int -> string) ->
  in_enc:encoding ->
  out_enc:encoding ->
  ?range_pos:int ->
  ?range_len:int ->
  string ->
  string
(** Converts the string from [in_enc] to [out_enc], and returns it.
* The string must consist of a whole number of characters. If it
* ends with an incomplete multi-byte character, however, this is
* detected, and the exception [Malformed_code] will be raised.
* This exception is also raised for other encoding errors in the
* input string.
*
* @param subst This function is invoked for code points of [in_enc] that
*   cannot be represented in [out_enc], and the result of the function 
*   invocation is substituted (directly, without any further conversion).
*   Restriction: The string returned by [subst] must not be longer than 50
*   bytes.
*   If [subst] is missing, [Cannot_represent] is raised in this case.
*
* @param range_pos Selects a substring for conversion. [range_pos]
*   is the byte position of the first character of the substring.
*   (Default: 0)
*
* @param range_len Selects a substring for conversion. [range_len]
*   is the length of the substring in bytes (Default: Length
*   of the input string minus [range_pos])
*)

val makechar : encoding -> int -> string
(** [makechar enc i:]
  * Creates the string representing the Unicode code point [i] in encoding
  * [enc]. Raises [Not_found] if the character is legal but cannot be 
  * represented in [enc].
  * 
  * Possible encodings: everything but [`Enc_utf16] and [`Enc_utf32]
  *
  * Evaluation hints:
  * - PRE_EVAL(encoding)
  *
  * @deprecated This function is deprecated since ocamlnet-0.96. Use
  *   [ustring_of_uchar] instead.
  *)
