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

exception Malformed_code
(** Raised when an illegal byte sequence is found *)

exception Cannot_represent of int
(** Raised when a certain Unicode code point cannot be represented in
   * the selected output encoding
   *)

type encoding =
  [ `Enc_utf8 (* UTF-8 *)
  | `Enc_utf8_opt_bom
  | `Enc_java (* The variant of UTF-8 used by Java *)
  | `Enc_utf16 (* UTF-16 with unspecified endianess (restricted usage) *)
  | `Enc_utf16_le (* UTF-16 little endian *)
  | `Enc_utf16_be (* UTF-16 big endian *)
  | `Enc_utf32 (* UTF-32 with unspecified endianess (restricted usage) *)
  | `Enc_utf32_le (* UTF-32 little endian *)
  | `Enc_utf32_be (* UTF-32 big endian *)
  | `Enc_usascii (* US-ASCII (only 7 bit) *)
  | `Enc_iso88591 (* ISO-8859-1 *)
  | `Enc_iso88592 (* ISO-8859-2 *)
  | `Enc_iso88593 (* ISO-8859-3 *)
  | `Enc_iso88594 (* ISO-8859-4 *)
  | `Enc_iso88595 (* ISO-8859-5 *)
  | `Enc_iso88596 (* ISO-8859-6 *)
  | `Enc_iso88597 (* ISO-8859-7 *)
  | `Enc_iso88598 (* ISO-8859-8 *)
  | `Enc_iso88599 (* ISO-8859-9 *)
  | `Enc_iso885910 (* ISO-8859-10 *)
  | `Enc_iso885911 (* ISO-8859-11 *)
  | `Enc_iso885913 (* ISO-8859-13 *)
  | `Enc_iso885914 (* ISO-8859-14 *)
  | `Enc_iso885915 (* ISO-8859-15 *)
  | `Enc_iso885916 (* ISO-8859-16 *)
  | `Enc_koi8r (* KOI8-R *)
  | `Enc_jis0201 (* JIS-X-0201 (Roman in lower half; Katakana upper half *)
  | `Enc_eucjp (* EUC-JP (includes US-ASCII, JIS-X-0201, -0208, -0212) *)
  | (* Japanese, TODO: *)
    (*|  `Enc_iso2022jp of jis_state = [ `Enc_usascii | `Enc_jis0201 |
                                         `Enc_jis0208_1978 | `Enc_jis0208_1893 ]
          It is very likely that ISO-2022 will be handled in a different module.
          This encoding is too weird.
      |  `Enc_sjis
    *)
    `Enc_euckr
    (* EUC-KR (includes US-ASCII, KS-X-1001) *)
  | (* Older standards: *)
    `Enc_asn1_iso646
    (* only the language-neutral subset - "IA5String" *)
  | `Enc_asn1_T61 (* ITU T.61 ("Teletex") *)
  | `Enc_asn1_printable (* ASN.1 Printable *)
  | (* Microsoft: *)
    `Enc_windows1250 (* WINDOWS-1250 *)
  | `Enc_windows1251 (* WINDOWS-1251 *)
  | `Enc_windows1252 (* WINDOWS-1252 *)
  | `Enc_windows1253 (* WINDOWS-1253 *)
  | `Enc_windows1254 (* WINDOWS-1254 *)
  | `Enc_windows1255 (* WINDOWS-1255 *)
  | `Enc_windows1256 (* WINDOWS-1256 *)
  | `Enc_windows1257 (* WINDOWS-1257 *)
  | `Enc_windows1258 (* WINDOWS-1258 *)
  | (* IBM, ASCII-based: *)
    `Enc_cp437
  | `Enc_cp737
  | `Enc_cp775
  | `Enc_cp850
  | `Enc_cp852
  | `Enc_cp855
  | `Enc_cp856
  | `Enc_cp857
  | `Enc_cp860
  | `Enc_cp861
  | `Enc_cp862
  | `Enc_cp863
  | `Enc_cp864
  | `Enc_cp865
  | `Enc_cp866
  | `Enc_cp869
  | `Enc_cp874
  | `Enc_cp1006
  | (* IBM, EBCDIC-based: *)
    `Enc_cp037
  | `Enc_cp424
  | `Enc_cp500
  | `Enc_cp875
  | `Enc_cp1026
  | `Enc_cp1047
  | (* Adobe: *)
    `Enc_adobe_standard_encoding
  | `Enc_adobe_symbol_encoding
  | `Enc_adobe_zapf_dingbats_encoding
  | (* Apple: *)
    `Enc_macroman
  | (* Encoding subset: *)
    `Enc_subset of encoding * (int -> bool)
  | `Enc_empty (* does not encode any character *) ]
(** The polymorphic variant enumerating the supported encodings. We have:
 * - [`Enc_utf8]: UTF-8
 * - [`Enc_utf8_opt_bom]: UTF-8 with an optional byte order mark at the
 *   beginning of the text
 * - [`Enc_java]: The UTF-8 variant used by Java (the only difference is
 *   the representation of NUL)
 * - [`Enc_utf16]: UTF-16 with unspecified endianess (restricted)
 * - [`Enc_utf16_le]: UTF-16 little endian
 * - [`Enc_utf16_be]: UTF-16 big endian
 * - [`Enc_utf32]: UTF-32 with unspecified endianess (restricted)
 * - [`Enc_utf32_le]: UTF-32 little endian
 * - [`Enc_utf32_be]: UTF-32 big endian
 * - [`Enc_usascii]: US-ASCII (7 bits)
 * - [`Enc_iso8859]{i n}: ISO-8859-{i n}
 * - [`Enc_koi8r]: KOI8-R
 * - [`Enc_jis0201]: JIS-X-0201 (Roman and Katakana)
 * - [`Enc_eucjp]: EUC-JP (code points from US-ASCII, JIS-X-0202, -0208, and
 *   -0212)
 * - [`Enc_euckr]: EUC-KR (code points from US-ASCII, KS-X-1001)
 * - [`Enc_windows]{i n}: WINDOWS-{i n}
 * - [`Enc_cp]{i n}: IBM code page {i n}. Note that there are both ASCII-
 *   and EBCDIC-based code pages
 * - [`Enc_adobe_*]: Adobe-specific encodings, e.g. used in Adobe fonts
 * - [`Enc_mac*]: Macintosh-specific encodings
 * - [`Enc_subset(e,def)]: The subset of [e] by applying the definition 
 *   function [def]
 * - [`Enc_empty]: The empty encoding (does not represent any character)
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

val is_ascii_compatible : encoding -> bool
(** "ASCII compatible" means: The bytes 1 to 127 represent the ASCII
  * codes 1 to 127, and no other representation of a character contains
  * the bytes 1 to 127.
  * 
  * For example, ISO-8859-1 is ASCII-compatible because the byte 1 to
  * 127 mean the same as in ASCII, and all other characters use bytes
  * greater than 127. UTF-8 is ASCII-compatible for the same reasons,
  * it does not matter that there are multi-byte characters.
  * EBCDIC is not ASCII-compatible because the bytes 1 to 127 do not mean
  * the same as in ASCII. UTF-16 is not ASCII-compatible because the bytes
  * 1 to 127 can occur in multi-byte representations of non-ASCII
  * characters.
  *
  * The byte 0 has been excluded from this definition because the C
  * language uses it with a special meaning that has nothing to do with
  * characters, so it is questionable to interpret the byte 0 anyway.
  *)

val is_single_byte : encoding -> bool
(** Returns whether the encoding is a single-byte encoding *)

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
