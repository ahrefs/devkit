open Netaux.ArrayAux

exception Malformed_code
exception Cannot_represent of int

let multibyte_limit = (* 6 *) 50

(* The longest multibyte character of all supported encodings,
 * and the longest substitution string.
 *)

let big_slice = (* 3 *) 250

(* The typical length of slices *)

(* Seems to be a good source: ftp://dkuug.dk/i18n/charmaps
  *)

type encoding =
  [ `Enc_utf8 (* UTF-8 *)
  | `Enc_utf8_opt_bom
  | `Enc_java
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
  | `Enc_koi8r
    (* KOI8-R *)
    (* http://koi8.pp.ru *)
  | (*|  `Enc_koi8u      (* KOI8-U *)   (* http://www.net.ua/KOI8-U/index.html *)*)
    `Enc_jis0201
    (* JIS-X-0201 *)
  | (*
   |  `Enc_jis0201_roman  (* JIS-X-0201 only roman half *)
   |  `Enc_jis0201_kana   (* JIS-X-0201 katakana half remapped to 0x21..XXX *)
   |  `Enc_jis0208_94x94  (* JIS-X-0208 in ISO-2022-style two byte encoding *)
   |  `Enc_jis0212_94x94  (* JIS-X-0212 in ISO-2022-style two byte encoding *)
 *)
    `Enc_eucjp
    (* EUC-JP *)
  | `Enc_euckr (* EUC-KR *)
  | (*
       |  `Enc_iso2022 of iso2022_state
       |  `Enc_iso2022jp of iso2022jp_state
    *)
    (* Older standards: *)
    `Enc_asn1_iso646
    (* only the language-neutral subset *)
  | `Enc_asn1_T61 (* ITU T.61 ("Teletex") *)
  | `Enc_asn1_printable
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
  | `Enc_cp866 (* Russian *)
  | `Enc_cp869
  | `Enc_cp874
  | `Enc_cp1006
  | (* IBM, EBCDIC-based: *)
    `Enc_cp037
    (* EBCDIC USA Canada *)
    (* 273: EBCDIC Germany, Austria,
     * 277: Denmark, Norway,
     * 278: Finland, Sweden,
     * 280: Italy,
     * 284: Spain, Latin America,
     * 285: United Kingdom,
     * 297: France,
     * 871: Iceland,
     *)
  | `Enc_cp424
  | `Enc_cp500 (* EBCDIC International *)
  | `Enc_cp875 (* EBCDIC Modern Greek *)
  | `Enc_cp1026 (* EBCDIC Turkish *)
  | `Enc_cp1047 (* EBCDIC Latin1, OS 390 System Services *)
  | (* Adobe: *)
    `Enc_adobe_standard_encoding
  | `Enc_adobe_symbol_encoding
  | `Enc_adobe_zapf_dingbats_encoding
  | (* Apple: *)
    `Enc_macroman
  | (* Encoding subset: *)
    `Enc_subset of encoding * (int -> bool)
  | `Enc_empty ]

type charset =
  [ `Set_unicode (* The full Unicode repertoire *)
  | `Set_usascii (* US-ASCII (only 7 bit) *)
  | `Set_iso88591 (* ISO-8859-1 *)
  | `Set_iso88592 (* ISO-8859-2 *)
  | `Set_iso88593 (* ISO-8859-3 *)
  | `Set_iso88594 (* ISO-8859-4 *)
  | `Set_iso88595 (* ISO-8859-5 *)
  | `Set_iso88596 (* ISO-8859-6 *)
  | `Set_iso88597 (* ISO-8859-7 *)
  | `Set_iso88598 (* ISO-8859-8 *)
  | `Set_iso88599 (* ISO-8859-9 *)
  | `Set_iso885910 (* ISO-8859-10 *)
  | `Set_iso885911 (* ISO-8859-11 *)
  | `Set_iso885913 (* ISO-8859-13 *)
  | `Set_iso885914 (* ISO-8859-14 *)
  | `Set_iso885915 (* ISO-8859-15 *)
  | `Set_iso885916 (* ISO-8859-16 *)
  | `Set_koi8r (* KOI8-R *)
  | `Set_jis0201 (* JIS-X-0201 *)
  | `Set_jis0208 (* JIS-X-0208 *)
  | `Set_jis0212 (* JIS-X-0212 *)
  | `Set_ks1001 (* KS-X-1001 *)
  | `Set_asn1_iso646
  | `Set_asn1_T61
  | `Set_asn1_printable
  | (* Microsoft: *)
    `Set_windows1250 (* WINDOWS-1250 *)
  | `Set_windows1251 (* WINDOWS-1251 *)
  | `Set_windows1252 (* WINDOWS-1252 *)
  | `Set_windows1253 (* WINDOWS-1253 *)
  | `Set_windows1254 (* WINDOWS-1254 *)
  | `Set_windows1255 (* WINDOWS-1255 *)
  | `Set_windows1256 (* WINDOWS-1256 *)
  | `Set_windows1257 (* WINDOWS-1257 *)
  | `Set_windows1258 (* WINDOWS-1258 *)
  | (* IBM, ASCII-based: *)
    `Set_cp437
  | `Set_cp737
  | `Set_cp775
  | `Set_cp850
  | `Set_cp852
  | `Set_cp855
  | `Set_cp856
  | `Set_cp857
  | `Set_cp860
  | `Set_cp861
  | `Set_cp862
  | `Set_cp863
  | `Set_cp864
  | `Set_cp865
  | `Set_cp866
  | `Set_cp869
  | `Set_cp874
  | `Set_cp1006
  | (* IBM, EBCDIC-based: *)
    `Set_cp037
  | `Set_cp424
  | `Set_cp500
  | `Set_cp875
  | `Set_cp1026
  | `Set_cp1047
  | (* Adobe: *)
    `Set_adobe_standard_encoding
  | `Set_adobe_symbol_encoding
  | `Set_adobe_zapf_dingbats_encoding
  | (* Apple: *)
    `Set_macroman ]

let ascii_compat_encodings =
  [
    `Enc_utf8;
    `Enc_utf8_opt_bom;
    `Enc_java;
    `Enc_usascii;
    `Enc_iso88591;
    `Enc_iso88592;
    `Enc_iso88593;
    `Enc_iso88594;
    `Enc_iso88595;
    `Enc_iso88596;
    `Enc_iso88597;
    `Enc_iso88598;
    `Enc_iso88599;
    `Enc_iso885910;
    `Enc_iso885911;
    `Enc_iso885913;
    `Enc_iso885914;
    `Enc_iso885915;
    `Enc_iso885916;
    `Enc_koi8r;
    `Enc_windows1250;
    `Enc_windows1251;
    `Enc_windows1252;
    `Enc_windows1253;
    `Enc_windows1254;
    `Enc_windows1255;
    `Enc_windows1256;
    `Enc_windows1257;
    `Enc_windows1258;
    `Enc_cp437;
    `Enc_cp737;
    `Enc_cp775;
    `Enc_cp850;
    `Enc_cp852;
    `Enc_cp855;
    `Enc_cp856;
    `Enc_cp857;
    `Enc_cp860;
    `Enc_cp861;
    `Enc_cp862;
    `Enc_cp863;
    `Enc_cp864;
    `Enc_cp865;
    `Enc_cp866;
    `Enc_cp869;
    `Enc_cp874;
    `Enc_cp1006;
    `Enc_eucjp;
    `Enc_euckr;
    `Enc_macroman;
  ]

let rec is_ascii_compatible = function
  | `Enc_subset (e, _) -> is_ascii_compatible e
  | e -> List.mem e ascii_compat_encodings

let rec is_single_byte = function
  | `Enc_utf8 | `Enc_utf8_opt_bom | `Enc_java | `Enc_utf16 | `Enc_utf16_le
  | `Enc_utf16_be | `Enc_utf32 | `Enc_utf32_le | `Enc_utf32_be ->
      false
  | `Enc_eucjp -> false
  | `Enc_euckr -> false
  | `Enc_subset (e, _) -> is_single_byte e
  | _ -> true

let internal_name (cs : charset) =
  (* The name used for netdb lookups *)
  match cs with
  | `Set_unicode -> "unicode"
  | `Set_usascii -> "usascii"
  | `Set_iso88591 -> "iso88591"
  | `Set_iso88592 -> "iso88592"
  | `Set_iso88593 -> "iso88593"
  | `Set_iso88594 -> "iso88594"
  | `Set_iso88595 -> "iso88595"
  | `Set_iso88596 -> "iso88596"
  | `Set_iso88597 -> "iso88597"
  | `Set_iso88598 -> "iso88598"
  | `Set_iso88599 -> "iso88599"
  | `Set_iso885910 -> "iso885910"
  | `Set_iso885911 -> "iso885911"
  | `Set_iso885913 -> "iso885913"
  | `Set_iso885914 -> "iso885914"
  | `Set_iso885915 -> "iso885915"
  | `Set_iso885916 -> "iso885916"
  | `Set_koi8r -> "koi8r"
  | `Set_jis0201 -> "jis0201"
  | `Set_jis0208 -> "jis0208"
  | `Set_jis0212 -> "jis0212"
  | `Set_ks1001 -> "ks1001"
  | `Set_asn1_iso646 -> "asn1_iso646"
  | `Set_asn1_T61 -> "asn1_t61"
  | `Set_asn1_printable -> "asn1_printable"
  | `Set_windows1250 -> "windows1250"
  | `Set_windows1251 -> "windows1251"
  | `Set_windows1252 -> "windows1252"
  | `Set_windows1253 -> "windows1253"
  | `Set_windows1254 -> "windows1254"
  | `Set_windows1255 -> "windows1255"
  | `Set_windows1256 -> "windows1256"
  | `Set_windows1257 -> "windows1257"
  | `Set_windows1258 -> "windows1258"
  | `Set_cp437 -> "cp437"
  | `Set_cp737 -> "cp737"
  | `Set_cp775 -> "cp775"
  | `Set_cp850 -> "cp850"
  | `Set_cp852 -> "cp852"
  | `Set_cp855 -> "cp855"
  | `Set_cp856 -> "cp856"
  | `Set_cp857 -> "cp857"
  | `Set_cp860 -> "cp860"
  | `Set_cp861 -> "cp861"
  | `Set_cp862 -> "cp862"
  | `Set_cp863 -> "cp863"
  | `Set_cp864 -> "cp864"
  | `Set_cp865 -> "cp865"
  | `Set_cp866 -> "cp866"
  | `Set_cp869 -> "cp869"
  | `Set_cp874 -> "cp874"
  | `Set_cp1006 -> "cp1006"
  | `Set_cp037 -> "cp037"
  | `Set_cp424 -> "cp424"
  | `Set_cp500 -> "cp500"
  | `Set_cp875 -> "cp875"
  | `Set_cp1026 -> "cp1026"
  | `Set_cp1047 -> "cp1047"
  | `Set_adobe_standard_encoding -> "adobe_standard_encoding"
  | `Set_adobe_symbol_encoding -> "adobe_symbol_encoding"
  | `Set_adobe_zapf_dingbats_encoding -> "adobe_zapf_dingbats_encoding"
  | `Set_macroman -> "macroman"

let rec required_charsets (e : encoding) =
  (* The name is a bit misleading. The function returns the charsets that
   * correspond to the conversion tables that are required to support the
   * encoding.
   *)
  match e with
  | `Enc_utf8 | `Enc_utf8_opt_bom | `Enc_java | `Enc_utf16 | `Enc_utf16_le
  | `Enc_utf16_be | `Enc_utf32 | `Enc_utf32_le | `Enc_utf32_be ->
      []
  | `Enc_usascii -> []
  | `Enc_iso88591 -> []
  | `Enc_iso88592 -> [ `Set_iso88592 ]
  | `Enc_iso88593 -> [ `Set_iso88593 ]
  | `Enc_iso88594 -> [ `Set_iso88594 ]
  | `Enc_iso88595 -> [ `Set_iso88595 ]
  | `Enc_iso88596 -> [ `Set_iso88596 ]
  | `Enc_iso88597 -> [ `Set_iso88597 ]
  | `Enc_iso88598 -> [ `Set_iso88598 ]
  | `Enc_iso88599 -> [ `Set_iso88599 ]
  | `Enc_iso885910 -> [ `Set_iso885910 ]
  | `Enc_iso885911 -> [ `Set_iso885911 ]
  | `Enc_iso885913 -> [ `Set_iso885913 ]
  | `Enc_iso885914 -> [ `Set_iso885914 ]
  | `Enc_iso885915 -> [ `Set_iso885915 ]
  | `Enc_iso885916 -> [ `Set_iso885916 ]
  | `Enc_koi8r -> [ `Set_koi8r ]
  | `Enc_jis0201 -> [ `Set_jis0201 ]
  | `Enc_eucjp -> [ `Set_jis0201; `Set_jis0208; `Set_jis0212 ]
  | `Enc_euckr -> [ `Set_ks1001 ]
  | `Enc_asn1_iso646 -> [ `Set_asn1_iso646 ]
  | `Enc_asn1_T61 -> [ `Set_asn1_T61 ]
  | `Enc_asn1_printable -> [ `Set_asn1_printable ]
  | `Enc_windows1250 -> [ `Set_windows1250 ]
  | `Enc_windows1251 -> [ `Set_windows1251 ]
  | `Enc_windows1252 -> [ `Set_windows1252 ]
  | `Enc_windows1253 -> [ `Set_windows1253 ]
  | `Enc_windows1254 -> [ `Set_windows1254 ]
  | `Enc_windows1255 -> [ `Set_windows1255 ]
  | `Enc_windows1256 -> [ `Set_windows1256 ]
  | `Enc_windows1257 -> [ `Set_windows1257 ]
  | `Enc_windows1258 -> [ `Set_windows1258 ]
  | `Enc_cp437 -> [ `Set_cp437 ]
  | `Enc_cp737 -> [ `Set_cp737 ]
  | `Enc_cp775 -> [ `Set_cp775 ]
  | `Enc_cp850 -> [ `Set_cp850 ]
  | `Enc_cp852 -> [ `Set_cp852 ]
  | `Enc_cp855 -> [ `Set_cp855 ]
  | `Enc_cp856 -> [ `Set_cp856 ]
  | `Enc_cp857 -> [ `Set_cp857 ]
  | `Enc_cp860 -> [ `Set_cp860 ]
  | `Enc_cp861 -> [ `Set_cp861 ]
  | `Enc_cp862 -> [ `Set_cp862 ]
  | `Enc_cp863 -> [ `Set_cp863 ]
  | `Enc_cp864 -> [ `Set_cp864 ]
  | `Enc_cp865 -> [ `Set_cp865 ]
  | `Enc_cp866 -> [ `Set_cp866 ]
  | `Enc_cp869 -> [ `Set_cp869 ]
  | `Enc_cp874 -> [ `Set_cp874 ]
  | `Enc_cp1006 -> [ `Set_cp1006 ]
  | `Enc_cp037 -> [ `Set_cp037 ]
  | `Enc_cp424 -> [ `Set_cp424 ]
  | `Enc_cp500 -> [ `Set_cp500 ]
  | `Enc_cp875 -> [ `Set_cp875 ]
  | `Enc_cp1026 -> [ `Set_cp1026 ]
  | `Enc_cp1047 -> [ `Set_cp1047 ]
  | `Enc_adobe_standard_encoding -> [ `Set_adobe_standard_encoding ]
  | `Enc_adobe_symbol_encoding -> [ `Set_adobe_symbol_encoding ]
  | `Enc_adobe_zapf_dingbats_encoding -> [ `Set_adobe_zapf_dingbats_encoding ]
  | `Enc_macroman -> [ `Set_macroman ]
  | `Enc_subset (e', _) -> required_charsets e'
  | `Enc_empty -> []

(* Internal conversion interface:
 *
 * let (n_char, n_byte, enc') = read_XXX slice_char slice_blen s_in p_in l_in:
 *
 *  - Scans the bytes from position p_in until the slice is decoded, but at
 *    most until the last position p_in+l_in-1 of the input string s_in, and 
 *    decodes the character for the selected encoding.
 *  - "slice_char" is a preallocated array of ints storing the code points
 *    of the characters. It is allowed that "slice_char" is only partially
 *    filled with characters. In this case, there must be a -1 after the
 *    last valid code point.
 *  - "slice_blen" is another "int array" with the same size as "slice_char".
 *    It contains the byte length of every character. It is initialized with
 *    a sequence of ones, so single-byte readers don't have to worry about
 *    this array.
 *  - Returns:
 *      * n_char: the number of decoded characters
 *      * n_byte: the number of scanned bytes ( <= l_in )
 *      * enc': the new encoding
 *  - In the case of multi-byte encodings it is possible that
 *    the last byte to read at position p_in+l_in-1 is the beginning of
 *    a character. This character is excluded from being decoded.
 *  - Errors: If an invalid byte sequence is found, the exception
 *    Malformed_code_read(_,_,_) is raised. The exception returns the
 *    triple (n_char, n_byte, enc') describing how much could be read
 *    before the reader ran into the bad sequence. slice_char and slice_blen
 *    are only partially initialized, with a (-1) at the end of slice_char.
 *
 * let (n_char, n_byte) = 
 *        write_XXX slice_char slice_pos slice_length s_out p_out l_out subst
 *
 *  - Writes the characters found in slice_char to s_out. Only the elements
 *    from slice_pos to slice_pos + slice_length -1 are written. The resulting
 *    bytes are written to s_out from byte position p_out to p_out+l_out-1.
 *  - There must not be a -1 (EOF mark) in the first slice_length characters
 *    of slice_char.
 *  - Only whole characters must be written.
 *  - For code points p that cannot be represented in the output
 *    encoding, the function subst is called. The function must return
 *    the (already encoded) string to substitute. This must be a small string.
 *  - Of course, p >= 0. As special case, p >= 0x110000 may be used to force
 *    that subst is called (it is assumed that max_int can be never
 *    represented).
 *  - Returns:
 *      * n_char: the number of processed characters
 *      * n_byte: the number of written bytes ( <= l_in )
 *
 * let (n_char, n_byte) =
 *        back_XXX s_in range_in range_in_len p_in n_char:
 *
 *  - The substring of s_in beginning at range_in and with length
 *    range_in_len is considered as the valid range
 *  - The cursor is at byte position p_in and goes n_char characters back
 *  - The routine returns:
 *      * n_char: the characters the cursor was actually moved backwards
 *      * n_byte: the bytes the cursor was actually moved backwards
 *  - The validity of the input encoding needs not to be checked
 *)

exception Malformed_code_read of (int * int * encoding);;

(* not exported! *)

Callback.register_exception "Netconversion.Malformed_code_read"
  (Malformed_code_read (0, 0, `Enc_empty))

(* Needed by netaccel_c.c *)

(* UNSAFE_OPT: A number of functions have been optimized by using
 * unsafe features of O'Caml (unsafe_get, unsafe_set, unsafe_chr).
 * These functions have been checked very carefully, and there are
 * a lot of comments arguing about the correctness of the array
 * and string accesses.
 *)

type poly_reader = {
  read :
    's.
    's Netstring_tstring.tstring_ops ->
    int array ->
    int array ->
    's ->
    int ->
    int ->
    int * int * encoding;
}

let read_iso88591 maxcode enc =
  (* UNSAFE_OPT *)
  let read ops slice_char slice_blen s_in p_in l_in =
    let open Netstring_tstring in
    assert (Array.length slice_char = Array.length slice_blen);
    assert (p_in >= 0 && p_in + l_in <= ops.length s_in && l_in >= 0);
    let m = min l_in (Array.length slice_char) in
    let m3 = m / 3 in
    for k3 = 0 to m3 - 1 do
      let k = 3 * k3 in
      (* let ch = Char.code s_in.[ p_in + k ] in *)
      let chars = ops.unsafe_get3 s_in (p_in + k) in
      let c0 = chars lsr 16 in
      let c1 = (chars lsr 8) land 0xff in
      let c2 = chars land 0xff in
      if c0 > maxcode then (
        slice_char.(k) <- -1;
        raise (Malformed_code_read (k, k, enc)));
      Array.unsafe_set slice_char k c0;
      if c1 > maxcode then (
        slice_char.(k + 1) <- -1;
        raise (Malformed_code_read (k + 1, k + 1, enc)));
      Array.unsafe_set slice_char (k + 1) c1;
      if c2 > maxcode then (
        slice_char.(k + 2) <- -1;
        raise (Malformed_code_read (k + 2, k + 2, enc)));
      Array.unsafe_set slice_char (k + 2) c2
    done;
    for k = 3 * m3 to m - 1 do
      let c0 = Char.code (ops.unsafe_get s_in (p_in + k)) in
      if c0 > maxcode then (
        slice_char.(k) <- -1;
        raise (Malformed_code_read (k, k, enc)));
      Array.unsafe_set slice_char k c0
    done;
    if m < Array.length slice_char then slice_char.(m) <- -1;
    (m, m, enc)
  in
  { read }

let read_iso88591_ref = ref read_iso88591

let get_8bit_to_unicode_map enc =
  let cs =
    match required_charsets enc with
    | [ cs ] -> cs
    | _ -> failwith "get_8bit_to_unicode_map"
  in
  let to_unicode = Netmappings.get_to_unicode (internal_name cs) in
  assert (Array.length to_unicode = 256);
  to_unicode

let read_8bit enc =
  let m_to_unicode = get_8bit_to_unicode_map enc in

  (* the 256-byte array mapping the character set to unicode *)
  let read ops slice_char slice_blen s_in p_in l_in =
    (* UNSAFE_OPT *)
    let open Netstring_tstring in
    assert (Array.length slice_char = Array.length slice_blen);
    assert (p_in >= 0 && p_in + l_in <= ops.length s_in && l_in >= 0);
    let m = min l_in (Array.length slice_char) in
    let m3 = m / 3 in
    for k3 = 0 to m3 - 1 do
      let k = 3 * k3 in
      let chars = ops.unsafe_get3 s_in k in
      let c0 = chars lsr 16 in
      let c1 = (chars lsr 8) land 0xff in
      let c2 = chars land 0xff in
      let c0_uni = Array.unsafe_get m_to_unicode c0 in
      if c0_uni < 0 then (
        slice_char.(k) <- -1;
        raise (Malformed_code_read (k, k, enc)));
      Array.unsafe_set slice_char k c0_uni;
      let c1_uni = Array.unsafe_get m_to_unicode c1 in
      if c1_uni < 0 then (
        slice_char.(k + 1) <- -1;
        raise (Malformed_code_read (k + 1, k + 1, enc)));
      Array.unsafe_set slice_char (k + 1) c1_uni;
      let c2_uni = Array.unsafe_get m_to_unicode c2 in
      if c2_uni < 0 then (
        slice_char.(k + 2) <- -1;
        raise (Malformed_code_read (k + 2, k + 2, enc)));
      Array.unsafe_set slice_char (k + 2) c2_uni
    done;
    for k = 3 * m3 to m - 1 do
      let c0 = Char.code (ops.get s_in k) in
      let c0_uni = Array.unsafe_get m_to_unicode c0 in
      if c0_uni < 0 then (
        slice_char.(k) <- -1;
        raise (Malformed_code_read (k, k, enc)));
      Array.unsafe_set slice_char k c0_uni
    done;
    if m < Array.length slice_char then slice_char.(m) <- -1;
    (m, m, enc)
  in
  { read }

let read_utf8 is_java =
  (* UNSAFE_OPT *)
  let read ops slice_char slice_blen s_in p_in l_in =
    let open Netstring_tstring in
    assert (Array.length slice_char = Array.length slice_blen);
    assert (p_in >= 0 && p_in + l_in <= ops.length s_in && l_in >= 0);

    (* k: counts the bytes
     * n: counts the characters
     *)
    let p = ref p_in in
    let p_max = p_in + l_in in
    let n = ref 0 in
    let n_ret = ref (-1) in

    let malformed_code () =
      slice_char.(!n) <- -1;
      raise (Malformed_code_read (!n, !p - p_in, `Enc_utf8))
    in

    let slice_length = Array.length slice_char in

    while !p < p_max && !n < slice_length do
      let k_inc =
        (* length of the character in bytes; 0 means: stop *)

        (* We know:
         * (1) p_in >= 0 ==> !p >= 0
         * (2) !p < p_max = p_in + l_in <= String.length s_in
         * ==> unsafe get ok
         *)
        (* match s_in.[k_in + k] with *)
        match ops.unsafe_get s_in !p with
        | '\000' ->
            if is_java then malformed_code ();
            (* slice_char.(n) <- 0; *)
            Array.unsafe_set slice_char !n 0;
            (* ok *)
            1
        | '\001' .. '\127' as x ->
            (* slice_char.(n) <- Char.code x; *)
            Array.unsafe_set slice_char !n (Char.code x);
            (* ok *)
            1
        | '\128' .. '\223' as x ->
            if !p + 1 >= p_max then 0
            else
              (* ==> !p+1 < p_max = p_in + l_in <= String.length s_in
               * ==> unsafe get ok
               *)
              let n1 = Char.code x in
              let n2 =
                (* Char.code (s_in.[!p + 1]) *)
                Char.code (ops.unsafe_get s_in (!p + 1))
              in
              if is_java && n1 = 0x80 && n2 = 0xc0 then (
                (* slice_char.(n) <- 0; *)
                Array.unsafe_set slice_char !n 0;
                (* ok *)
                2)
              else (
                if n2 < 128 || n2 > 191 then malformed_code ();
                let p = ((n1 land 0b11111) lsl 6) lor (n2 land 0b111111) in
                if p < 128 then malformed_code ();
                (* slice_char.(n) <- p; *)
                Array.unsafe_set slice_char !n p;
                (* ok *)
                2)
        | '\224' .. '\239' as x ->
            if !p + 2 >= p_max then 0
            else
              (* ==> !p+2 < p_max = p_in + l_in <= String.length s_in
               * ==> unsafe get ok
               *)
              let n1 = Char.code x in
              let n2 =
                (* Char.code (s_in.[!p + 1]) *)
                Char.code (ops.unsafe_get s_in (!p + 1))
              in
              let n3 =
                (* Char.code (s_in.[!p + 2]) *)
                Char.code (ops.unsafe_get s_in (!p + 2))
              in
              if n2 < 128 || n2 > 191 then malformed_code ();
              if n3 < 128 || n3 > 191 then malformed_code ();
              let p =
                ((n1 land 0b1111) lsl 12)
                lor ((n2 land 0b111111) lsl 6)
                lor (n3 land 0b111111)
              in
              if p < 0x800 then malformed_code ();
              if p >= 0xd800 && p < 0xe000 then
                (* Surrogate pairs are not supported in UTF-8 *)
                malformed_code ();
              if p >= 0xfffe && p <= 0xffff then malformed_code ();
              (* slice_char.(n) <- p; *)
              Array.unsafe_set slice_char !n p;
              (* ok *)
              3
        | '\240' .. '\247' as x ->
            if !p + 3 >= p_max then 0
            else
              (* ==> !p+3 < p_max = p_in + l_in <= String.length s_in
               * ==> unsafe get ok
               *)
              let n1 = Char.code x in
              let chars = ops.unsafe_get3 s_in (!p + 1) in
              let n2 = chars lsr 16 in
              let n3 = (chars lsr 8) land 0xff in
              let n4 = chars land 0xff in
              if n2 < 128 || n2 > 191 then malformed_code ();
              if n3 < 128 || n3 > 191 then malformed_code ();
              if n4 < 128 || n4 > 191 then malformed_code ();
              let p =
                ((n1 land 0b111) lsl 18)
                lor ((n2 land 0b111111) lsl 12)
                lor ((n3 land 0b111111) lsl 6)
                lor (n4 land 0b111111)
              in
              if p < 0x10000 then malformed_code ();
              if p >= 0x110000 then
                (* These code points are not supported. *)
                malformed_code ();
              (* slice_char.(n) <- p; *)
              Array.unsafe_set slice_char !n p;
              (* ok *)
              4
        | _ ->
            (* Outside the valid range of XML characters *)
            malformed_code ()
      in

      (* If k_inc = 0, the character was partially outside the processed
       * range of the string, and could not be decoded.
       *)
      if k_inc > 0 then (
        (* We know:
         * (1) n >= 0, because n starts with 0 and is only increased
         * (2) n < Array.length slice_char = Array.length slice_blen
         * ==> unsafe set ok
         *)
        (* slice_blen.(n) <- k_inc; *)
        Array.unsafe_set slice_blen !n k_inc;
        (* next iteration: *)
        p := !p + k_inc;
        incr n)
      else (
        (* Stop loop: *)
        n_ret := !n;
        n := slice_length)
    done;

    if !n_ret = -1 then n_ret := !n;
    if !n_ret < slice_length then (* EOF marker *)
      slice_char.(!n_ret) <- -1;
    (!n_ret, !p - p_in, `Enc_utf8)
  in
  { read }

let read_utf8_ref = ref read_utf8

let have_utf8_bom ops s p =
  let open Netstring_tstring in
  let c0 = ops.get s (p + 0) in
  let c1 = ops.get s (p + 1) in
  let c2 = ops.get s (p + 2) in
  c0 = '\xEF' && c1 = '\xBB' && c2 = '\xBF'

let read_utf8_opt_bom expose_bom =
  let read ops slice_char slice_blen s_in p_in l_in =
    let open Netstring_tstring in
    assert (Array.length slice_char = Array.length slice_blen);
    assert (p_in >= 0 && p_in + l_in <= ops.length s_in && l_in >= 0);
    (* Expect a BOM at the beginning of the text *)
    if l_in >= 3 then
      if have_utf8_bom ops s_in p_in then (
        let p_in1, l_in1 =
          if expose_bom then (p_in, l_in) else (p_in + 3, l_in - 3)
        in
        let n_ret, p_ret, enc =
          (!read_utf8_ref false).read ops slice_char slice_blen s_in p_in1 l_in1
        in
        let p_ret1 = if expose_bom then p_ret else p_ret + 3 in
        if expose_bom && n_ret >= 1 then slice_char.(0) <- -3;
        (n_ret, p_ret1, enc))
      else (!read_utf8_ref false).read ops slice_char slice_blen s_in p_in l_in
    else
      let bom_possible =
        l_in = 0
        || (l_in = 1 && ops.get s_in 0 = '\xEF')
        || (l_in = 2 && ops.get s_in 0 = '\xEF' && ops.get s_in 1 = '\xBB')
      in
      if bom_possible then (0, 0, `Enc_utf8_opt_bom)
      else (!read_utf8_ref false).read ops slice_char slice_blen s_in p_in l_in
  in
  { read }

let surrogate_offset = 0x10000 - (0xD800 lsl 10) - 0xDC00

let read_utf16_lebe lo hi n_start enc =
  (* lo=0, hi=1: little endian
   * lo=1, hi=0: big endian
   * n_start: First cell in slice to use
   *)
  let read ops slice_char slice_blen s_in p_in l_in =
    let open Netstring_tstring in
    assert (Array.length slice_char = Array.length slice_blen);
    assert (p_in >= 0 && p_in + l_in <= ops.length s_in && l_in >= 0);

    let malformed_code k n =
      slice_char.(n) <- -1;
      raise (Malformed_code_read (n, k, enc))
    in

    (* k: counts the bytes
     * n: counts the characters
     *)
    let rec put_loop k n =
      if k + 1 < l_in && n < Array.length slice_char then
        let p =
          Char.code (ops.get s_in (p_in + k + lo))
          lor (Char.code (ops.get s_in (p_in + k + hi)) lsl 8)
        in

        if p >= 0xd800 && p < 0xe000 then
          (* This is a surrogate pair. *)
          if k + 3 < l_in then
            if p <= 0xdbff then (
              let q =
                Char.code (ops.get s_in (p_in + k + 2 + lo))
                lor (Char.code (ops.get s_in (p_in + k + 2 + hi)) lsl 8)
              in
              if q < 0xdc00 || q > 0xdfff then malformed_code k n;
              let eff_p = (p lsl 10) + q + surrogate_offset in
              slice_char.(n) <- eff_p;
              slice_blen.(n) <- 4;
              put_loop (k + 4) (n + 1))
            else (* Malformed pair: *)
              malformed_code k n
          else (n, k)
        else if (* Normal 2-byte character *)
                p = 0xfffe then
          (* Wrong byte order mark: It is illegal here *)
          malformed_code k n
        else (
          (* A regular code point *)
          slice_char.(n) <- p;
          slice_blen.(n) <- 2;
          put_loop (k + 2) (n + 1))
      else (n, k)
    in
    let n, k = put_loop 0 n_start in
    if n < Array.length slice_char then (* EOF marker *)
      slice_char.(n) <- -1;
    (n, k, enc)
  in
  { read }

let get_endianess ops s_in p_in =
  let open Netstring_tstring in
  let c0 = ops.get s_in (p_in + 0) in
  let c1 = ops.get s_in (p_in + 1) in
  if c0 = '\254' && c1 = '\255' then `Big_endian
  else if c0 = '\255' && c1 = '\254' then `Little_endian
  else `No_BOM

(* expose_bom: when true, the BOM is considered as a character and
 * put as value (-3) into slice_char
 *)

let read_utf16 expose_bom =
  let read ops slice_char slice_blen s_in p_in l_in =
    let open Netstring_tstring in
    assert (Array.length slice_char = Array.length slice_blen);
    assert (p_in >= 0 && p_in + l_in <= ops.length s_in && l_in >= 0);
    (* Expect a BOM at the beginning of the text *)
    if l_in >= 2 then (
      if expose_bom then (
        slice_char.(0) <- -3;
        slice_blen.(0) <- 0 (* Later corrected *));
      match get_endianess ops s_in p_in with
      | `Big_endian ->
          let n_start = if expose_bom then 1 else 0 in
          let n, k, enc' =
            (read_utf16_lebe 1 0 n_start `Enc_utf16_be).read ops slice_char
              slice_blen s_in (p_in + 2) (l_in - 2)
          in
          if n > 0 then slice_blen.(0) <- slice_blen.(0) + 2;
          (n, k + 2, enc')
      | `Little_endian ->
          let n_start = if expose_bom then 1 else 0 in
          let n, k, enc' =
            (read_utf16_lebe 0 1 n_start `Enc_utf16_le).read ops slice_char
              slice_blen s_in (p_in + 2) (l_in - 2)
          in
          if n > 0 then slice_blen.(0) <- slice_blen.(0) + 2;
          (n, k + 2, enc')
      | `No_BOM ->
          (* byte order mark missing *)
          slice_char.(0) <- -1;
          raise (Malformed_code_read (0, 0, `Enc_utf16)))
    else (
      slice_char.(0) <- -1;
      (0, 0, `Enc_utf16))
  in
  { read }

let read_utf32_lebe little n_start enc =
  (* little: whether little endian
   * n_start: First cell in slice to use
   *)
  let read ops slice_char slice_blen s_in p_in l_in =
    let open Netstring_tstring in
    assert (Array.length slice_char = Array.length slice_blen);
    assert (p_in >= 0 && p_in + l_in <= ops.length s_in && l_in >= 0);

    let malformed_code k n =
      slice_char.(n) <- -1;
      raise (Malformed_code_read (n, k, enc))
    in

    let b0 = if little then 0 else 3 in
    let b1 = if little then 1 else 2 in
    let b2 = if little then 2 else 1 in
    let b3 = if little then 3 else 0 in

    (* k: counts the bytes
     * n: counts the characters
     *)
    let rec put_loop k n =
      if k + 3 < l_in && n < Array.length slice_char then (
        let p3 = Char.code (ops.get s_in (p_in + k + b3)) in
        if p3 <> 0 then malformed_code k n;
        let p =
          Char.code (ops.get s_in (p_in + k + b0))
          lor (Char.code (ops.get s_in (p_in + k + b1)) lsl 8)
          lor (Char.code (ops.get s_in (p_in + k + b2)) lsl 16)
        in
        if (p >= 0xD800 && p <= 0xDFFF) || p >= 0x10FFFF then malformed_code k n;
        if p = 0xfffe then
          (* Wrong byte order mark: It is illegal here *)
          malformed_code k n;
        slice_char.(n) <- p;
        slice_blen.(n) <- 4;
        put_loop (k + 4) (n + 1))
      else (n, k)
    in
    let n, k = put_loop 0 n_start in
    if n < Array.length slice_char then (* EOF marker *)
      slice_char.(n) <- -1;
    (n, k, enc)
  in
  { read }

let get_endianess32 ops s_in p_in =
  let open Netstring_tstring in
  let c0 = ops.get s_in (p_in + 0) in
  let c1 = ops.get s_in (p_in + 1) in
  let c2 = ops.get s_in (p_in + 2) in
  let c3 = ops.get s_in (p_in + 3) in
  if c0 = '\000' && c1 = '\000' && c2 = '\254' && c3 = '\255' then `Big_endian
  else if c0 = '\255' && c1 = '\254' && c2 = '\000' && c3 = '\000' then
    `Little_endian
  else `No_BOM

let read_utf32 expose_bom =
  let read ops slice_char slice_blen s_in p_in l_in =
    let open Netstring_tstring in
    assert (Array.length slice_char = Array.length slice_blen);
    assert (p_in >= 0 && p_in + l_in <= ops.length s_in && l_in >= 0);
    (* Expect a BOM at the beginning of the text *)
    if l_in >= 4 then (
      if expose_bom then (
        slice_char.(0) <- -3;
        slice_blen.(0) <- 0 (* Later corrected *));
      match get_endianess32 ops s_in p_in with
      | `Big_endian ->
          let n_start = if expose_bom then 1 else 0 in
          let n, k, enc' =
            (read_utf32_lebe false n_start `Enc_utf32_be).read ops slice_char
              slice_blen s_in (p_in + 4) (l_in - 4)
          in
          if n > 0 then slice_blen.(0) <- slice_blen.(0) + 4;
          (n, k + 4, enc')
      | `Little_endian ->
          let n_start = if expose_bom then 1 else 0 in
          let n, k, enc' =
            (read_utf32_lebe true n_start `Enc_utf32_le).read ops slice_char
              slice_blen s_in (p_in + 4) (l_in - 4)
          in
          if n > 0 then slice_blen.(0) <- slice_blen.(0) + 4;
          (n, k + 4, enc')
      | `No_BOM ->
          (* byte order mark missing *)
          slice_char.(0) <- -1;
          raise (Malformed_code_read (0, 0, `Enc_utf32)))
    else (
      slice_char.(0) <- -1;
      (0, 0, `Enc_utf32))
  in
  { read }

let read_euc len1 len2 len3 map1 map2 map3 enc =
  (* Code set 0 is US-ASCII.
   * Code sets 1, 2, 3 may be anything. lenX = 0: code set is not supported.
   * lenX is either 0, 1, or 2.
   *)
  (* UNSAFE_OPT *)
  let open Netstring_tstring in
  assert (len1 >= 0 && len1 <= 2);
  assert (len2 >= 0 && len2 <= 2);
  assert (len3 >= 0 && len3 <= 2);

  let read ops slice_char slice_blen s_in p_in l_in =
    assert (Array.length slice_char = Array.length slice_blen);
    assert (p_in >= 0 && p_in + l_in <= ops.length s_in && l_in >= 0);

    (* k: counts the bytes
     * n: counts the characters
     *)
    let p = ref p_in in
    let p_max = p_in + l_in in
    let n = ref 0 in
    let n_ret = ref (-1) in

    let malformed_code () =
      slice_char.(!n) <- -1;
      raise (Malformed_code_read (!n, !p - p_in, enc))
    in

    let slice_length = Array.length slice_char in

    while !p < p_max && !n < slice_length do
      let k_inc =
        (* length of the character in bytes; 0 means: stop *)

        (* We know:
         * (1) p_in >= 0 ==> !p >= 0
         * (2) !p < p_max = p_in + l_in <= String.length s_in
         * ==> unsafe get ok
         *)
        (* match s_in.[k_in + k] with *)
        match ops.unsafe_get s_in !p with
        | '\000' .. '\127' as x ->
            (* US-ASCII *)
            Array.unsafe_set slice_char !n (Char.code x);
            (* ok *)
            1
        | '\142' ->
            (* Code set 2 *)
            if len2 = 0 then malformed_code ();
            if !p + len2 >= p_max then 0
            else
              let x1 = Char.code (ops.get s_in (!p + 1)) in
              let x2 =
                if len2 = 1 then 256 else Char.code (ops.get s_in (!p + 2))
              in
              if x1 < 160 || x2 < 160 then malformed_code ();
              let uni = map2 x1 x2 in
              Array.unsafe_set slice_char !n uni;
              (* ok *)
              len2 + 1
        | '\143' ->
            (* Code set 3 *)
            if len3 = 0 then malformed_code ();
            if !p + len3 >= p_max then 0
            else
              let x1 = Char.code (ops.get s_in (!p + 1)) in
              let x2 =
                if len3 = 1 then 256 else Char.code (ops.get s_in (!p + 2))
              in
              if x1 < 160 || x2 < 160 then malformed_code ();
              let uni = map3 x1 x2 in
              Array.unsafe_set slice_char !n uni;
              (* ok *)
              len3 + 1
        | '\160' .. '\255' as x1_code ->
            (* Code set 1 *)
            if !p + len1 > p_max then 0
            else
              let x1 = Char.code x1_code in
              let x2 =
                if len1 = 1 then 256 else Char.code (ops.get s_in (!p + 1))
              in
              if x2 < 160 then malformed_code ();
              let uni = map1 x1 x2 in
              Array.unsafe_set slice_char !n uni;
              (* ok *)
              len1
        | _ ->
            (* illegal *)
            malformed_code ()
      in

      (* If k_inc = 0, the character was partially outside the processed
       * range of the string, and could not be decoded.
       *)
      if k_inc > 0 then (
        (* We know:
         * (1) n >= 0, because n starts with 0 and is only increased
         * (2) n < Array.length slice_char = Array.length slice_blen
         * ==> unsafe set ok
         *)
        (* slice_blen.(n) <- k_inc; *)
        Array.unsafe_set slice_blen !n k_inc;
        (* next iteration: *)
        p := !p + k_inc;
        incr n)
      else (
        (* Stop loop: *)
        n_ret := !n;
        n := slice_length)
    done;

    if !n_ret = -1 then n_ret := !n;
    if !n_ret < slice_length then (* EOF marker *)
      slice_char.(!n_ret) <- -1;
    (!n_ret, !p - p_in, enc)
  in
  { read }

let read_eucjp () =
  let jis0201 = Netmappings.get_to_unicode "jis0201" in
  let jis0208 = Netmappings.get_to_unicode "jis0208" in
  let jis0212 = lazy (Netmappings.get_to_unicode "jis0212") in
  (* seldom *)
  let map1 x1 x2 = jis0208.(((x1 - 160) * 96) + x2 - 160) in
  let map2 x1 _ = jis0201.(x1) in
  let map3 x1 x2 = (Lazy.force jis0212).(((x1 - 160) * 96) + x2 - 160) in
  read_euc 2 1 2 map1 map2 map3 `Enc_eucjp

let read_euckr () =
  let ks1001 = Netmappings.get_to_unicode "ks1001" in
  let map x1 x2 = ks1001.(((x1 - 160) * 96) + x2 - 160) in
  read_euc 2 0 0 map map map `Enc_euckr

let read_subset inner_read def =
  let read ops slice_char slice_blen s_in p_in l_in =
    let open Netstring_tstring in
    assert (Array.length slice_char = Array.length slice_blen);
    assert (p_in >= 0 && p_in + l_in <= ops.length s_in && l_in >= 0);

    let n, k, enc' = inner_read.read ops slice_char slice_blen s_in p_in l_in in

    (* check codepoints: *)
    for j = 0 to n - 1 do
      if not (def slice_char.(j)) then (
        (* raise Malformed_code_read... *)
        (* to get enc'' read again: *)
        let slice_char' = Array.make j (-1) in
        let slice_blen' = Array.make j 1 in
        let n', k', enc'' =
          try inner_read.read ops slice_char' slice_blen' s_in p_in l_in
          with Malformed_code_read (_, _, _) -> assert false
        in
        assert (n' = j);
        int_blit slice_char' 0 slice_char 0 j;
        int_blit slice_blen' 0 slice_blen 0 j;
        slice_char.(j) <- -1;
        raise (Malformed_code_read (j, k', enc'')))
    done;

    (n, k, enc')
  in
  { read }

(*
 * let (n_char, b_byte) = 
 *        write_XXX slice_char slice_length s_out p_out l_out subst
 *)

let write_iso88591 maxcode slice_char slice_pos slice_length s_out p_out l_out
    subst =
  (* UNSAFE_OPT *)
  (* Use maxcode=255 for ISO-8859-1, and maxcode=127 for US-ASCII,
   * and maxcode=(-1) for `Enc_empty.
   *)
  assert (p_out >= 0 && p_out + l_out <= Bytes.length s_out && l_out >= 0);
  assert (slice_pos >= 0 && slice_pos + slice_length <= Array.length slice_char);
  assert (maxcode <= 255);

  let n = ref slice_pos in
  (* index of slice *)
  let n_ret = ref (-1) in
  (* returned number of characters *)
  let n_max = slice_pos + slice_length in

  let p = ref p_out in
  (* current output position *)
  let p_max = p_out + l_out in

  (* maximum output position *)
  while !n < n_max && !p < p_max do
    (* We know:
     * (1) !n >= 0, because it starts with 0 and is only increased
     * (2) !n < n_max = slice_pos + slice_length <= Array.length slice_char
     * ==> unsafe get ok
     *)
    let ch = Array.unsafe_get slice_char !n in
    if ch >= 0 && ch <= maxcode then (
      (* Because !p < p_max:
       * !p < p_max = p_out + l_out <= String.length s_out
       * Furthermore, p_out >= 0, !p >= 0.
       * ==> unsafe set ok
       *)
      (* s_out.[ !p ] <- Char.chr ch; *)
      Bytes.unsafe_set s_out !p (Char.unsafe_chr ch);
      incr n;
      incr p)
    else (
      assert (ch >= 0);
      let replacement = subst ch in
      let l_repl = String.length replacement in
      if l_repl > multibyte_limit then
        failwith "Netconversion.write_iso88591: Substitution string too long";
      if !p + l_repl <= p_max then (
        (* Enough space to store 'replacement': *)
        Bytes.blit_string replacement 0 s_out !p l_repl;
        p := !p + l_repl;
        incr n)
      else (
        (* Exit whole conversion *)
        n_ret := !n;
        n := n_max))
  done;
  if !n_ret >= 0 then (!n_ret - slice_pos, !p - p_out)
  else (!n - slice_pos, !p - p_out)

let get_8bit_from_unicode_map enc =
  let cs =
    match required_charsets enc with
    | [ cs ] -> cs
    | _ -> failwith "get_8bit_from_unicode_map"
  in
  let from_unicode = Netmappings.get_from_unicode (internal_name cs) in
  assert (Array.length from_unicode = 256);
  from_unicode

let write_8bit enc =
  (* UNSAFE_OPT *)
  let m_from_unicode = get_8bit_from_unicode_map enc in
  let m_mask = Array.length m_from_unicode - 1 in

  fun slice_char slice_pos slice_length s_out p_out l_out subst ->
    assert (p_out >= 0 && p_out + l_out <= Bytes.length s_out && l_out >= 0);
    assert (
      slice_pos >= 0 && slice_pos + slice_length <= Array.length slice_char);

    let n = ref slice_pos in
    (* index of slice *)
    let n_max = slice_pos + slice_length in

    let k = ref 0 in
    (* written bytes *)
    let n_ret = ref (-1) in

    (* returned number of characters *)
    while !n < n_max && !k < l_out do
      (* We know:
       * (1) !n >= 0, because it starts with 0 and is only increased
       * (2) !n < n_max = slice_pos + slice_length <= Array.length slice
       * ==> unsafe get ok
       *)
      let p =
        (* slice_char.( !n ) *)
        Array.unsafe_get slice_char !n
      in
      let p' =
        match Array.unsafe_get m_from_unicode (p land m_mask) with
        | Netmappings.U_nil -> -1
        | Netmappings.U_single (p0, q0) -> if p0 = p then q0 else -1
        | Netmappings.U_double (p0, q0, p1, q1) ->
            if p0 = p then q0 else if p1 = p then q1 else -1
        | Netmappings.U_array pq ->
            let r = ref (-1) in
            let h = ref 0 in
            while !r < 0 && !h < Array.length pq do
              if pq.(!h) = p then r := pq.(!h + 1) else h := !h + 2
            done;
            !r
      in

      (* If p=-1 ==> p'=-1, because -1 is never mapped to any code point *)
      if p' < 0 then (
        if p < 0 then assert false (* EOF mark found *)
        else
          let replacement = subst p in
          let l_repl = String.length replacement in
          if l_repl > multibyte_limit then
            failwith "Netconversion.write_8bit: Substitution string too long";

          if !k + l_repl <= l_out then (
            (* Enough space to store 'replacement': *)
            Bytes.blit_string replacement 0 s_out (p_out + !k) l_repl;
            k := !k + l_repl;
            incr n)
          else (
            (* Exit whole conversion *)
            n_ret := !n;
            n := n_max))
      else (
        (* Because !k < l_out:
         * p_out + !k < p_out + l_out <= String.length s_out
         * Furthermore, p_out >= 0, !k >= 0.
         * ==> unsafe set ok
         *)
        (* s_out.[ p_out + !k ] <- Char.chr p'; *)
        Bytes.unsafe_set s_out (p_out + !k) (Char.unsafe_chr (p' land 0xff));
        incr n;
        incr k)
    done;
    if !n_ret >= 0 then (!n_ret - slice_pos, !k) else (!n - slice_pos, !k)

let write_utf8 is_java slice_char slice_pos slice_length s_out p_out l_out subst
    =
  (* UNSAFE_OPT *)
  assert (p_out >= 0 && p_out + l_out <= Bytes.length s_out && l_out >= 0);
  assert (slice_pos >= 0 && slice_pos + slice_length <= Array.length slice_char);

  let n = ref slice_pos in
  (* index of slice *)
  let n_max = slice_pos + slice_length in

  let k = ref 0 in
  (* written bytes *)
  let n_ret = ref (-1) in

  (* returned number of characters *)
  while !n < n_max do
    (* We know:
     * (1) !n >= 0, because it starts with 0 and is only increased
     * (2) !n < n_max = slice_pos + slice_length <= Array.length slice
     * ==> unsafe get ok
     *)
    let p =
      (* slice.( !n ) *)
      Array.unsafe_get slice_char !n
    in

    let index = p_out + !k in

    let k_inc =
      (* k_inc: how many bytes are written. (-1) means: stop *)
      if p <= 127 && ((not is_java) || p <> 0) then (
        if p < 0 then assert false;
        (* EOF mark *)
        if !k < l_out then (
          (* (1) index = p_out + !k < p_out + l_out <= 
           *     String.length s_out
           * (2) p_out, !n >= 0
           * ==> unsafe set ok
           *
           * 0 <= p <= 127 ==> unsafe_chr ok
           *)
          (* s_out.[index] <- Char.chr p; *)
          Bytes.unsafe_set s_out index (Char.unsafe_chr p);
          1)
        else -1)
      else if p <= 0x7ff then
        if !k + 1 < l_out then (
          (* (1) index+1 = p_out + !k + 1 < p_out + l_out <= 
           *     String.length s_out
           * (2) p_out, !k >= 0
           * ==> unsafe set ok
           *
           * p <= 0x7ff ==> p lsr 6 <= 0x1f 
           *            ==> 0xc0 lor (p lsr 6) <= df
           * p land 0x3f <= 0x3f ==> 0x80 lor (p land 0x3f) <= 0xbf
           * ==> unsafe_chr ok
           *)
          (* s_out.[index]     <- Char.chr (0xc0 lor (p lsr 6)); *)
          (* s_out.[index + 1] <- Char.chr (0x80 lor (p land 0x3f)); *)
          Bytes.unsafe_set s_out index (Char.unsafe_chr (0xc0 lor (p lsr 6)));
          Bytes.unsafe_set s_out (index + 1)
            (Char.unsafe_chr (0x80 lor (p land 0x3f)));
          2)
        else -1
      else if p <= 0xffff then (
        (* Refuse writing surrogate pairs, and fffe, ffff *)
        if (p >= 0xd800 && p < 0xe000) || p >= 0xfffe then
          failwith "Netconversion.write_utf8";
        if !k + 2 < l_out then (
          (* (1) index+2 = p_out + !k + 2 < p_out + l_out <= 
           *     String.length s_out
           * (2) p_out, !k >= 0
           * ==> unsafe set ok
           *
           * Well, and it can be proven that unsafe_chr is ok, too...
           *)
          (* s_out.[index]     <- Char.chr (0xe0 lor (p lsr 12)); *)
          (* s_out.[index + 1] <- Char.chr (0x80 lor ((p lsr 6) land 0x3f)); *)
          (* s_out.[index + 2] <- Char.chr (0x80 lor (p land 0x3f)); *)
          Bytes.unsafe_set s_out index (Char.unsafe_chr (0xe0 lor (p lsr 12)));
          Bytes.unsafe_set s_out (index + 1)
            (Char.unsafe_chr (0x80 lor ((p lsr 6) land 0x3f)));
          Bytes.unsafe_set s_out (index + 2)
            (Char.unsafe_chr (0x80 lor (p land 0x3f)));
          3)
        else -1)
      else if p <= 0x10ffff then
        if !k + 3 < l_out then (
          (* No such characters are defined... *)
          Bytes.set s_out index (Char.chr (0xf0 lor (p lsr 18)));
          Bytes.set s_out (index + 1)
            (Char.chr (0x80 lor ((p lsr 12) land 0x3f)));
          Bytes.set s_out (index + 2)
            (Char.chr (0x80 lor ((p lsr 6) land 0x3f)));
          Bytes.set s_out (index + 3) (Char.chr (0x80 lor (p land 0x3f)));
          4)
        else -1
      else
        (* Higher code points are not possible in XML; call subst *)
        let replacement = subst p in
        let l_repl = String.length replacement in
        if l_repl > multibyte_limit then
          failwith "Netconversion.write_utf8: Substitution string too long";
        if !k + l_repl <= l_out then (
          (* Enough space to store 'replacement': *)
          Bytes.blit_string replacement 0 s_out (p_out + !k) l_repl;
          l_repl (* may be 0! *))
        else -1 (* Exit whole conversion *)
    in

    if k_inc >= 0 then (
      k := !k + k_inc;
      incr n)
    else (
      n_ret := !n;
      n := n_max)
  done;
  if !n_ret >= 0 then (!n_ret - slice_pos, !k) else (!n - slice_pos, !k)

let write_utf16_lebe lo hi slice_char slice_pos slice_length s_out p_out l_out
    subst =
  (* lo=0, hi=1: little endian
   * lo=1, hi=0: big endian
   *)
  assert (p_out >= 0 && p_out + l_out <= Bytes.length s_out && l_out >= 0);
  assert (slice_pos >= 0 && slice_pos + slice_length <= Array.length slice_char);

  let n = ref slice_pos in
  (* index of slice *)
  let n_max = slice_pos + slice_length in

  let k = ref 0 in
  (* written bytes *)
  let n_ret = ref (-1) in

  (* returned number of characters *)
  while !n < n_max do
    let p = slice_char.(!n) in

    let index = p_out + !k in

    let k_inc =
      if p >= 0xfffe then (
        if p <= 0x10ffff then (
          if p <= 0xffff then failwith "Netconversion.write_utf16_le";
          (* Must be written as surrogate pair *)
          if !k + 3 < l_out then (
            let high = ((p - 0x10000) lsr 10) + 0xd800 in
            let low = (p land 0x3ff) + 0xdc00 in
            Bytes.set s_out (index + lo) (Char.chr (high land 0xff));
            Bytes.set s_out (index + hi) (Char.chr (high lsr 8));
            Bytes.set s_out (index + 2 + lo) (Char.chr (low land 0xff));
            Bytes.set s_out (index + 2 + hi) (Char.chr (low lsr 8));
            4)
          else -1)
        else
          (* Higher code points are not possible in XML; call subst *)
          let replacement = subst p in
          let l_repl = String.length replacement in
          if l_repl > multibyte_limit then
            failwith
              "Netconversion.write_utf16_le: Substitution string too long";
          if !k + l_repl <= l_out then (
            (* Enough space to store 'replacement': *)
            Bytes.blit_string replacement 0 s_out (p_out + !k) l_repl;
            l_repl (* may be 0! *))
          else -1 (* Exit whole conversion *))
      else if (* 2-byte character *)
              !k + 1 < l_out then (
        Bytes.set s_out (index + lo) (Char.unsafe_chr (p land 0xff));
        Bytes.set s_out (index + hi) (Char.unsafe_chr ((p lsr 8) land 0xff));
        2)
      else -1
    in
    if k_inc >= 0 then (
      k := !k + k_inc;
      incr n)
    else (
      n_ret := !n;
      n := n_max)
  done;
  if !n_ret >= 0 then (!n_ret - slice_pos, !k) else (!n - slice_pos, !k)

let write_utf32_lebe little slice_char slice_pos slice_length s_out p_out l_out
    subst =
  assert (p_out >= 0 && p_out + l_out <= Bytes.length s_out && l_out >= 0);
  assert (slice_pos >= 0 && slice_pos + slice_length <= Array.length slice_char);

  let n = ref slice_pos in
  (* index of slice *)
  let n_max = slice_pos + slice_length in

  let k = ref 0 in
  (* written bytes *)
  let n_ret = ref (-1) in

  (* returned number of characters *)
  let b0 = if little then 0 else 3 in
  let b1 = if little then 1 else 2 in
  let b2 = if little then 2 else 1 in
  let b3 = if little then 3 else 0 in

  while !n < n_max do
    let p = slice_char.(!n) in

    let index = p_out + !k in

    let k_inc =
      if p <= 0x10ffff then
        if !k + 3 < l_out then (
          Bytes.set s_out (index + b0) (Char.unsafe_chr (p land 0xff));
          Bytes.set s_out (index + b1) (Char.unsafe_chr ((p lsr 8) land 0xff));
          Bytes.set s_out (index + b2) (Char.unsafe_chr ((p lsr 16) land 0xff));
          Bytes.set s_out (index + b3) (Char.unsafe_chr 0);
          4)
        else -1
      else
        (* Higher code points are not possible in XML; call subst *)
        let replacement = subst p in
        let l_repl = String.length replacement in
        if l_repl > multibyte_limit then
          failwith "Netconversion.write_utf32: Substitution string too long";
        if !k + l_repl <= l_out then (
          (* Enough space to store 'replacement': *)
          Bytes.blit_string replacement 0 s_out (p_out + !k) l_repl;
          l_repl (* may be 0! *))
        else -1
      (* Exit whole conversion *)
    in
    if k_inc >= 0 then (
      k := !k + k_inc;
      incr n)
    else (
      n_ret := !n;
      n := n_max)
  done;
  if !n_ret >= 0 then (!n_ret - slice_pos, !k) else (!n - slice_pos, !k)

let write_euc map _enc
    (* Code set 0 is US-ASCII.
     * let (set, byte1, byte2) = map unicode:
     *  - set is 1, 2, 3, or 4. 4 means that the code point cannot be mapped.
     *  - byte1 >= 160, <= 255
     *  - byte2 >= 160, <= 255, or byte2=256 meaning that it is not used
     *)
    (* UNSAFE_OPT *)
      slice_char slice_pos slice_length s_out p_out l_out subst =
  assert (p_out >= 0 && p_out + l_out <= Bytes.length s_out && l_out >= 0);
  assert (slice_pos >= 0 && slice_pos + slice_length <= Array.length slice_char);

  let n = ref slice_pos in
  (* index of slice *)
  let n_max = slice_pos + slice_length in

  let k = ref 0 in
  (* written bytes *)
  let n_ret = ref (-1) in

  (* returned number of characters *)
  while !n < n_max do
    (* We know:
     * (1) !n >= 0, because it starts with 0 and is only increased
     * (2) !n < n_max = slice_pos + slice_length <= Array.length slice
     * ==> unsafe get ok
     *)
    let p =
      (* slice.( !n ) *)
      Array.unsafe_get slice_char !n
    in
    assert (p >= 0);

    let index = p_out + !k in

    let set, b1, b2 = if p <= 127 then (0, p, 256) else map p in

    let k_inc =
      (* k_inc: how many bytes are written *)
      match set with
      | 0 ->
          if !k < l_out then (
            (* s_out.[index] <- Char.chr p; *)
            Bytes.unsafe_set s_out index (Char.unsafe_chr (b1 land 127));
            1)
          else -1
      | 1 ->
          let bl = if b2 = 256 then 1 else 2 in
          if !k + bl < l_out then (
            assert (b1 >= 160 && b1 <= 255 && b2 >= 160 && b2 <= 256);
            Bytes.set s_out index (Char.chr b1);
            if b2 <> 256 then Bytes.set s_out (index + 1) (Char.chr b2);
            bl)
          else -1
      | 2 ->
          let bl = if b2 = 256 then 2 else 3 in
          if !k + bl < l_out then (
            assert (b1 >= 160 && b1 <= 255 && b2 >= 160 && b2 <= 256);
            Bytes.set s_out index '\142';
            Bytes.set s_out (index + 1) (Char.chr b1);
            if b2 <> 256 then Bytes.set s_out (index + 2) (Char.chr b2);
            bl)
          else -1
      | 3 ->
          let bl = if b2 = 256 then 2 else 3 in
          if !k + bl < l_out then (
            assert (b1 >= 160 && b1 <= 255 && b2 >= 160 && b2 <= 256);
            Bytes.set s_out index '\143';
            Bytes.set s_out (index + 1) (Char.chr b1);
            if b2 <> 256 then Bytes.set s_out (index + 2) (Char.chr b2);
            bl)
          else -1
      | 4 ->
          let replacement = subst p in
          let l_repl = String.length replacement in
          if l_repl > multibyte_limit then
            failwith "Netconversion.write_euc: Substitution string too long";
          if !k + l_repl <= l_out then (
            (* Enough space to store 'replacement': *)
            Bytes.blit_string replacement 0 s_out (p_out + !k) l_repl;
            l_repl)
          else -1
          (* Exit whole conversion *)
      | _ -> assert false
    in
    if k_inc >= 0 then (
      k := !k + k_inc;
      incr n)
    else (
      n_ret := !n;
      n := n_max)
  done;
  if !n_ret >= 0 then (!n_ret - slice_pos, !k) else (!n - slice_pos, !k)

let write_eucjp () =
  let jis0201 = Netmappings.get_from_unicode "jis0201" in
  let jis0208 = Netmappings.get_from_unicode "jis0208" in
  let jis0212 = Netmappings.get_from_unicode "jis0212" in

  let jis0201_mask = Array.length jis0201 - 1 in
  let jis0208_mask = Array.length jis0208 - 1 in
  let jis0212_mask = Array.length jis0212 - 1 in

  let map p =
    (* Try in order: jis0208, jis0201, jis0212 *)
    let map_tbl jistbl jistbl_mask =
      match jistbl.(p land jistbl_mask) with
      | Netmappings.U_nil -> -1
      | Netmappings.U_single (p0, q0) -> if p0 = p then q0 else -1
      | Netmappings.U_double (p0, q0, p1, q1) ->
          if p0 = p then q0 else if p1 = p then q1 else -1
      | Netmappings.U_array pq ->
          let r = ref (-1) in
          let h = ref 0 in
          while !r < 0 && !h < Array.length pq do
            if pq.(!h) = p then r := pq.(!h + 1) else h := !h + 2
          done;
          !r
    in
    let cp_0208 = map_tbl jis0208 jis0208_mask in
    if cp_0208 >= 0 then
      let row = cp_0208 / 96 in
      let col = cp_0208 - (row * 96) in
      (1, row + 160, col + 160)
    else
      let cp_0201 = map_tbl jis0201 jis0201_mask in
      if cp_0201 >= 128 then (* Ignore especially 0x5c, 0x7e *)
        (2, cp_0201, 256)
      else
        let cp_0212 = map_tbl jis0212 jis0212_mask in
        if cp_0212 >= 0 then
          let row = cp_0212 / 96 in
          let col = cp_0212 - (row * 96) in
          (3, row + 160, col + 160)
        else (4, 256, 256)
  in
  write_euc map `Enc_eucjp

let write_euckr () =
  let ks1001 = Netmappings.get_from_unicode "ks1001" in

  let ks1001_mask = Array.length ks1001 - 1 in

  let map p =
    let map_tbl kstbl kstbl_mask =
      match kstbl.(p land kstbl_mask) with
      | Netmappings.U_nil -> -1
      | Netmappings.U_single (p0, q0) -> if p0 = p then q0 else -1
      | Netmappings.U_double (p0, q0, p1, q1) ->
          if p0 = p then q0 else if p1 = p then q1 else -1
      | Netmappings.U_array pq ->
          let r = ref (-1) in
          let h = ref 0 in
          while !r < 0 && !h < Array.length pq do
            if pq.(!h) = p then r := pq.(!h + 1) else h := !h + 2
          done;
          !r
    in
    let cp_1001 = map_tbl ks1001 ks1001_mask in
    if cp_1001 >= 0 then
      let row = cp_1001 / 96 in
      let col = cp_1001 - (row * 96) in
      (1, row + 160, col + 160)
    else (4, 256, 256)
  in
  write_euc map `Enc_euckr

let special_cpoint = 0x110000

let write_subset inner_writer def slice_char slice_pos slice_length s_out p_out
    l_out subst =
  assert (p_out >= 0 && p_out + l_out <= Bytes.length s_out && l_out >= 0);
  assert (slice_pos >= 0 && slice_pos + slice_length <= Array.length slice_char);

  (* Force that the subst' function is called for all undefined code
   * points
   *)
  let slice_char' = Array.sub slice_char slice_pos slice_length in
  for n = 0 to slice_length - 1 do
    let ch = slice_char'.(n) in
    if ch >= special_cpoint || not (def ch) then
      slice_char'.(n) <- special_cpoint + n
  done;

  let subst' ch =
    if ch >= special_cpoint then
      subst slice_char.(slice_pos + ch - special_cpoint)
    else subst ch
  in

  inner_writer slice_char' 0 slice_length s_out p_out l_out subst'

type encoding1 = [ encoding | `Enc_utf16_bom | `Enc_utf32_bom | `Enc_utf8_bom ]

(* `Enc_*_bom considers the BOM as a character with code point -3.
 * This encoding is only internally used.
 *)

let rec get_reader1 (enc : encoding1) =
  (* get_reader1 supports the additional internal encodings of
   * encoding1. get_reader (below) only supports the exported
   * encodings.
   *)
  match enc with
  | `Enc_iso88591 -> !read_iso88591_ref 255 `Enc_iso88591
  | `Enc_usascii -> !read_iso88591_ref 127 `Enc_usascii
  | `Enc_empty -> !read_iso88591_ref (-1) `Enc_empty
  | `Enc_utf8 -> !read_utf8_ref false
  | `Enc_java -> !read_utf8_ref true
  | `Enc_utf8_opt_bom -> read_utf8_opt_bom false
  | `Enc_utf8_bom -> read_utf8_opt_bom true
  | `Enc_utf16 -> read_utf16 false
  | `Enc_utf16_bom -> read_utf16 true
  | `Enc_utf16_le -> read_utf16_lebe 0 1 0 `Enc_utf16_le
  | `Enc_utf16_be -> read_utf16_lebe 1 0 0 `Enc_utf16_be
  | `Enc_utf32 -> read_utf32 false
  | `Enc_utf32_bom -> read_utf32 true
  | `Enc_utf32_le -> read_utf32_lebe true 0 `Enc_utf32_le
  | `Enc_utf32_be -> read_utf32_lebe false 0 `Enc_utf32_be
  | `Enc_eucjp -> read_eucjp ()
  | `Enc_euckr -> read_euckr ()
  | `Enc_subset (e, def) ->
      let reader' = get_reader1 (e :> encoding1) in
      read_subset reader' def
  | #encoding as e -> read_8bit (e :> encoding)

let get_reader = (get_reader1 : encoding1 -> 'a :> encoding -> 'a)

let rec get_writer enc =
  match enc with
  | `Enc_iso88591 -> write_iso88591 255
  | `Enc_usascii -> write_iso88591 127
  | `Enc_empty -> write_iso88591 (-1)
  | `Enc_utf8 -> write_utf8 false
  | `Enc_java -> write_utf8 true
  | `Enc_utf16 ->
      failwith
        "Netconversion: Cannot output text as `Enc_utf16, use `Enc_utf16_le or \
         `Enc_utf16_be"
  | `Enc_utf16_le -> write_utf16_lebe 0 1
  | `Enc_utf16_be -> write_utf16_lebe 1 0
  | `Enc_utf32 ->
      failwith
        "Netconversion: Cannot output text as `Enc_utf32, use `Enc_utf32_le or \
         `Enc_utf32_be"
  | `Enc_utf32_le -> write_utf32_lebe true
  | `Enc_utf32_be -> write_utf32_lebe false
  | `Enc_eucjp -> write_eucjp ()
  | `Enc_euckr -> write_euckr ()
  | `Enc_subset (e, def) ->
      let writer' = get_writer e in
      write_subset writer' def
  | _ -> write_8bit enc

let recode_poly ~in_ops ~in_enc ~in_buf ~in_pos ~in_len ~out_enc ~out_buf
    ~out_pos ~out_len ~max_chars ~subst =
  let open Netstring_tstring in
  if
    in_pos < 0 || in_len < 0
    || in_pos + in_len > in_ops.length in_buf
    || out_pos < 0 || out_len < 0
    || out_pos + out_len > Bytes.length out_buf
  then invalid_arg "Netconversion.recode";

  (* An array with 250 elements can be allocated in the minor heap. *)
  let slice_length = big_slice in
  let slice_char = Array.make slice_length (-1) in
  let slice_blen = Array.make slice_length 1 in

  let in_k = ref 0 in
  (* read bytes *)
  let in_n = ref 0 in
  (* read characters *)
  let in_eof = ref (!in_k >= in_len) in
  let out_k = ref 0 in
  (* written bytes *)
  let out_n = ref 0 in
  (* written characters *)
  let out_eof = ref (!out_k >= out_len || !out_n >= max_chars) in

  let rd_enc = ref in_enc in
  let reader = ref (get_reader in_enc) in
  let writer = get_writer out_enc in

  while (not !in_eof) && not !out_eof do
    let in_n_inc, in_k_inc, rd_enc' =
      try
        !reader.read in_ops slice_char slice_blen in_buf (in_pos + !in_k)
          (in_len - !in_k)
      with Malformed_code_read (in_n_inc, in_k_inc, rd_enc') ->
        if in_n_inc = 0 then raise Malformed_code;
        (in_n_inc, in_k_inc, rd_enc')
    in

    let out_n_inc_max = min in_n_inc (max_chars - !out_n) in
    (* do not write more than max_chars *)
    let out_n_inc, out_k_inc =
      if out_n_inc_max > 0 then
        writer slice_char 0 out_n_inc_max out_buf (out_pos + !out_k)
          (out_len - !out_k) subst
      else (0, 0)
    in

    let in_n_inc', in_k_inc' =
      if in_n_inc > out_n_inc then (
        (* Not all read characters could be written *)
        let sum = ref 0 in
        for j = 0 to out_n_inc - 1 do
          sum := !sum + slice_blen.(j)
        done;
        (out_n_inc, !sum))
      else (in_n_inc, in_k_inc)
    in

    in_k := !in_k + in_k_inc';
    in_n := !in_n + in_n_inc';
    out_k := !out_k + out_k_inc;
    out_n := !out_n + out_n_inc;

    (* Detect change of input encoding: *)
    if rd_enc' <> !rd_enc then (
      rd_enc := rd_enc';
      reader := get_reader rd_enc';
      Array.fill slice_blen 0 slice_length 1);

    (* EOF criteria:
     * - It is possible that !in_k never reaches in_len because there is a
     *   multibyte character at the end that is partially outside the input
     *   range
     * - For the same reason it is possible that !out_k never reaches out_len
     * - It is accepted as reader EOF if not even one character can be
     *   scanned
     * - It is accepted as writer EOF if fewer than in_n_inc characters
     *   could be written
     *)
    in_eof := !in_k >= in_len || in_n_inc = 0;

    out_eof := !out_k >= out_len || !out_n >= max_chars || out_n_inc < in_n_inc
  done;

  (!in_k, !out_k, !rd_enc)

let rec ustring_of_uchar enc =
  let multi_byte writer n p =
    let s = Bytes.create n in
    let _, n_act =
      writer [| p |] 0 1 s 0 n (fun _ -> raise (Cannot_represent p))
    in
    Bytes.sub_string s 0 n_act
  in
  match enc with
  | `Enc_iso88591 ->
      fun p ->
        if p > 255 then raise (Cannot_represent p);
        String.make 1 (Char.chr p)
  | `Enc_usascii ->
      fun p ->
        if p > 127 then raise (Cannot_represent p);
        String.make 1 (Char.chr p)
  | `Enc_utf8 | `Enc_utf8_opt_bom -> multi_byte (write_utf8 false) 4
  | `Enc_java -> multi_byte (write_utf8 true) 4
  | `Enc_utf16_le -> multi_byte (write_utf16_lebe 0 1) 4
  | `Enc_utf16_be -> multi_byte (write_utf16_lebe 1 0) 4
  | `Enc_utf16 ->
      invalid_arg "Netconversion.ustring_of_uchar: UTF-16 not possible"
  | `Enc_utf32_le -> multi_byte (write_utf32_lebe true) 4
  | `Enc_utf32_be -> multi_byte (write_utf32_lebe false) 4
  | `Enc_utf32 ->
      invalid_arg "Netconversion.ustring_of_uchar: UTF-32 not possible"
  | `Enc_eucjp -> multi_byte (write_eucjp ()) 3
  | `Enc_euckr -> multi_byte (write_euckr ()) 2
  | `Enc_subset (e, def) ->
      fun p ->
        if def p then ustring_of_uchar e p else raise (Cannot_represent p)
  | _ ->
      let writer = write_8bit enc in
      multi_byte writer 1

let makechar enc =
  let us = ustring_of_uchar enc in
  fun p -> try us p with Cannot_represent _ -> raise Not_found

(* The following algorithms assume that there is an upper limit of the length
 * of a multibyte character. Currently, UTF8 is the encoding with the longest
 * multibyte characters (6 bytes).
 * Because of this limit, it is allowed to allocate a buffer that is "large
 * enough" in order to ensure that at least one character is recoded in every
 * loop cycle. If the buffer was not large enough, no character would be
 * processed in a cycle, and the algorithm would hang.
 *)

let convert_poly :
    type s t.
    in_ops:s Netstring_tstring.tstring_ops ->
    out_kind:t Netstring_tstring.tstring_kind ->
    ?subst:(int -> string) ->
    in_enc:encoding ->
    out_enc:encoding ->
    ?range_pos:int ->
    ?range_len:int ->
    s ->
    t =
 fun ~in_ops ~out_kind ?(subst = fun p -> raise (Cannot_represent p)) ~in_enc
     ~out_enc ?(range_pos = 0) ?range_len s ->
  let open Netstring_tstring in
  let range_len =
    match range_len with Some l -> l | None -> in_ops.length s - range_pos
  in

  if range_pos < 0 || range_len < 0 || range_pos + range_len > in_ops.length s
  then invalid_arg "Netconversion.convert";

  (* Estimate the size of the output string: 
   * length * 2 is just guessed. It is assumed that this number is usually
   * too large, and to avoid that too much memory is wasted, the buffer is
   * limited by 10000.
   *)
  let size = ref (max multibyte_limit (min 10000 (range_len * 2))) in
  let out_buf = ref (Bytes.create !size) in

  let k_in = ref 0 in
  let k_out = ref 0 in

  while !k_in < range_len do
    let in_len = range_len - !k_in in
    let out_len = !size - !k_out in
    assert (out_len >= multibyte_limit);
    (* space for at least one char *)
    let k_in_inc, k_out_inc, _in_enc' =
      recode_poly ~in_ops ~in_enc ~in_buf:s ~in_pos:(range_pos + !k_in) ~in_len
        ~out_enc ~out_buf:!out_buf ~out_pos:!k_out ~out_len ~max_chars:max_int
        ~subst
    in
    if k_in_inc = 0 then raise Malformed_code;
    (* Reasons for k_in_inc = 0:
     * (1) There is not enough space in out_buf to add a single character
     * (2) in_buf ends with a prefix of a multi-byte character
     * Because there is always space for at least one character
     * ( = multibyte_limit ), reason (1) can be excluded. So it must
     * be (2), and we can raise Malformed_code.
     *)
    k_in := !k_in + k_in_inc;
    k_out := !k_out + k_out_inc;
    (* double the size of out_buf: *)
    let size' = min Sys.max_string_length (!size + !size) in
    if size' < !size + multibyte_limit then
      failwith "Netconversion.convert: string too long";
    let out_buf' = Bytes.create size' in
    Bytes.blit !out_buf 0 out_buf' 0 !k_out;
    out_buf := out_buf';
    size := size'
  done;
  match out_kind with
  | Netstring_tstring.String_kind -> Bytes.sub_string !out_buf 0 !k_out
  | Netstring_tstring.Bytes_kind -> Bytes.sub !out_buf 0 !k_out

let convert ?subst ~in_enc ~out_enc ?range_pos ?range_len s =
  convert_poly ?subst ~in_ops:Netstring_tstring.string_ops
    ~out_kind:Netstring_tstring.String_kind ~in_enc ~out_enc ?range_pos
    ?range_len s

let uarray_of_ustring_poly ops enc ?(range_pos = 0) ?range_len s =
  let open Netstring_tstring in
  let range_len =
    match range_len with Some l -> l | None -> ops.length s - range_pos
  in

  if range_pos < 0 || range_len < 0 || range_pos + range_len > ops.length s then
    invalid_arg "Netconversion.uarray_of_ustring";

  let slice_length = big_slice in
  let slice_char = Array.make slice_length (-1) in
  let slice_blen = Array.make slice_length 1 in

  let k = ref 0 in
  let e = ref enc in
  let reader = ref (get_reader enc) in
  let buf = ref [] in

  while !k < range_len do
    let n_inc, k_inc, enc' =
      try
        !reader.read ops slice_char slice_blen s (range_pos + !k)
          (range_len - !k)
      with Malformed_code_read (_, _, _) -> raise Malformed_code
    in

    k := !k + k_inc;
    buf := Array.sub slice_char 0 n_inc :: !buf;

    if enc' <> !e then (
      e := enc';
      reader := get_reader enc';
      Array.fill slice_blen 0 slice_length 1);

    if n_inc < slice_length then (
      (* EOF *)
      if !k < range_len then raise Malformed_code;
      (* s ends with multi-byte prefix*)
      k := range_len)
  done;

  Array.concat (List.rev !buf)

let uarray_of_ustring enc =
  uarray_of_ustring_poly Netstring_tstring.string_ops enc

let ustring_of_uarray_poly out_kind
    ?(subst = fun code -> raise (Cannot_represent code)) enc ?(pos = 0) ?len ua
    =
  let len = match len with Some l -> l | None -> Array.length ua - pos in

  if pos < 0 || len < 0 || pos + len > Array.length ua then
    invalid_arg "Netconversion.ustring_of_uarray";

  (* Estimate the size of the output string: 
   * length * 2 is just guessed. It is assumed that this number is usually
   * too large, and to avoid that too much memory is wasted, the buffer is
   * limited by 10000.
   *)
  let size = ref (max multibyte_limit (min 10000 (len * 2))) in
  let out_buf = ref (Bytes.create !size) in

  let writer = get_writer enc in

  let k_in = ref 0 in
  let k_out = ref 0 in

  while !k_in < len do
    let k_in_inc, k_out_inc =
      writer ua (pos + !k_in) (len - !k_in) !out_buf !k_out (!size - !k_out)
        subst
    in
    k_in := !k_in + k_in_inc;
    k_out := !k_out + k_out_inc;

    (* double the size of out_buf: *)
    let size' = min Sys.max_string_length (!size + !size) in
    if size' < !size + multibyte_limit then
      failwith "Netconversion.ustring_of_uarray: string too long";
    let out_buf' = Bytes.create size' in
    Bytes.blit !out_buf 0 out_buf' 0 !k_out;
    out_buf := out_buf';
    size := size'
  done;

  Netstring_tstring.bytes_subpoly out_kind !out_buf 0 !k_out

let ustring_of_uarray ?subst =
  ustring_of_uarray_poly Netstring_tstring.String_kind ?subst
