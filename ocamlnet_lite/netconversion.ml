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
  | (* Encoding subset: *)
    `Enc_subset of encoding * (int -> bool) ]

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

exception Malformed_code_read of (int * int * encoding)

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

(* `Enc_*_bom considers the BOM as a character with code point -3.
 * This encoding is only internally used.
 *)

let rec get_reader1 (enc : encoding) =
  (* get_reader1 supports the additional internal encodings of
   * encoding1. get_reader (below) only supports the exported
   * encodings.
   *)
  match enc with
  | `Enc_utf8 -> !read_utf8_ref false
  | `Enc_subset (e, def) ->
      let reader' = get_reader1 e in
      read_subset reader' def

let get_reader = (get_reader1 : encoding -> 'a :> encoding -> 'a)

let rec get_writer enc =
  match enc with
  | `Enc_utf8 -> write_utf8 false
  | `Enc_subset (e, def) ->
      let writer' = get_writer e in
      write_subset writer' def

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
  | `Enc_utf8 -> multi_byte (write_utf8 false) 4
  | `Enc_subset (e, def) ->
      fun p ->
        if def p then ustring_of_uchar e p else raise (Cannot_represent p)

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
