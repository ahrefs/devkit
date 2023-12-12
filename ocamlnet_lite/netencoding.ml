module Url = struct
  (* adapted from https://gitlab.com/gerdstolpmann/lib-ocamlnet3/-/blob/4d1a8401bd40c17632128545e2aa4c880535e208/code/src/netstring/netencoding.ml#L993 *)
  let hex_digits =
    [|
      '0';
      '1';
      '2';
      '3';
      '4';
      '5';
      '6';
      '7';
      '8';
      '9';
      'A';
      'B';
      'C';
      'D';
      'E';
      'F';
    |]

  let to_hex2 k =
    (* Converts k to a 2-digit hex string *)
    let s = Bytes.create 2 in
    Bytes.set s 0 hex_digits.((k lsr 4) land 15);
    Bytes.set s 1 hex_digits.(k land 15);
    Bytes.unsafe_to_string s

  let of_hex1 c =
    match c with
    | '0' .. '9' -> Char.code c - Char.code '0'
    | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
    | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
    | _ -> raise Not_found

  let url_encoding_re = Netstring_str.regexp "[^A-Za-z0-9_.!*-]"
  let url_decoding_re = Netstring_str.regexp "\\+\\|%..\\|%.\\|%"

  let encode ?(plus = true) s =
    Netstring_str.global_substitute url_encoding_re
      (fun r _ ->
        match Netstring_str.matched_string r s with
        | " " when plus -> "+"
        | x ->
            let k = Char.code x.[0] in
            "%" ^ to_hex2 k)
      s

  let decode ?(plus = true) ?(pos = 0) ?len s =
    let s_l = String.length s in
    let s1 =
      if pos = 0 && len = None then s
      else
        let len = match len with Some n -> n | None -> s_l in
        String.sub s pos len
    in
    let l = String.length s1 in
    Netstring_str.global_substitute url_decoding_re
      (fun r _ ->
        match Netstring_str.matched_string r s1 with
        | "+" -> if plus then " " else "+"
        | _ -> (
            let i = Netstring_str.match_beginning r in
            (* Assertion: s1.[i] = '%' *)
            if i + 2 >= l then failwith "Web.Url.decode";
            let c1 = s1.[i + 1] in
            let c2 = s1.[i + 2] in
            try
              let k1 = of_hex1 c1 in
              let k2 = of_hex1 c2 in
              String.make 1 (Char.chr ((k1 lsl 4) lor k2))
            with Not_found -> failwith "Web.Url.decode"))
      s1

  let url_split_re = Netstring_str.regexp "[&=]"

  let dest_url_encoded_parameters parstr =
    let rec parse_after_amp tl =
      match tl with
      | Netstring_str.Text name
        :: Netstring_str.Delim "="
        :: Netstring_str.Text value
        :: tl' ->
          (decode name, decode value) :: parse_next tl'
      | Netstring_str.Text name
        :: Netstring_str.Delim "="
        :: Netstring_str.Delim "&"
        :: tl' ->
          (decode name, "") :: parse_after_amp tl'
      | [ Netstring_str.Text name; Netstring_str.Delim "=" ] ->
          [ (decode name, "") ]
      | _ -> failwith "Web.Url.dest_url_encoded_parameters"
    and parse_next tl =
      match tl with
      | [] -> []
      | Netstring_str.Delim "&" :: tl' -> parse_after_amp tl'
      | _ -> failwith "Web.Url.dest_url_encoded_parameters"
    in
    let toklist = Netstring_str.full_split url_split_re parstr in
    match toklist with [] -> [] | _ -> parse_after_amp toklist
end

module Html = struct
  let etable =
    [
      ("lt", 60);
      ("gt", 62);
      ("amp", 38);
      ("quot", 34);
      (* Note: &quot; is new in HTML-4.0, but it has been widely used
         * much earlier.
      *)
      ("apos", 39);
      (* Only used if contained in unsafe_chars *)
      (* ISO-8859-1: *)
      ("nbsp", 160);
      ("iexcl", 161);
      ("cent", 162);
      ("pound", 163);
      ("curren", 164);
      ("yen", 165);
      ("brvbar", 166);
      ("sect", 167);
      ("uml", 168);
      ("copy", 169);
      ("ordf", 170);
      ("laquo", 171);
      ("not", 172);
      ("shy", 173);
      ("reg", 174);
      ("macr", 175);
      ("deg", 176);
      ("plusmn", 177);
      ("sup2", 178);
      ("sup3", 179);
      ("acute", 180);
      ("micro", 181);
      ("para", 182);
      ("middot", 183);
      ("cedil", 184);
      ("sup1", 185);
      ("ordm", 186);
      ("raquo", 187);
      ("frac14", 188);
      ("frac12", 189);
      ("frac34", 190);
      ("iquest", 191);
      ("Agrave", 192);
      ("Aacute", 193);
      ("Acirc", 194);
      ("Atilde", 195);
      ("Auml", 196);
      ("Aring", 197);
      ("AElig", 198);
      ("Ccedil", 199);
      ("Egrave", 200);
      ("Eacute", 201);
      ("Ecirc", 202);
      ("Euml", 203);
      ("Igrave", 204);
      ("Iacute", 205);
      ("Icirc", 206);
      ("Iuml", 207);
      ("ETH", 208);
      ("Ntilde", 209);
      ("Ograve", 210);
      ("Oacute", 211);
      ("Ocirc", 212);
      ("Otilde", 213);
      ("Ouml", 214);
      ("times", 215);
      ("Oslash", 216);
      ("Ugrave", 217);
      ("Uacute", 218);
      ("Ucirc", 219);
      ("Uuml", 220);
      ("Yacute", 221);
      ("THORN", 222);
      ("szlig", 223);
      ("agrave", 224);
      ("aacute", 225);
      ("acirc", 226);
      ("atilde", 227);
      ("auml", 228);
      ("aring", 229);
      ("aelig", 230);
      ("ccedil", 231);
      ("egrave", 232);
      ("eacute", 233);
      ("ecirc", 234);
      ("euml", 235);
      ("igrave", 236);
      ("iacute", 237);
      ("icirc", 238);
      ("iuml", 239);
      ("eth", 240);
      ("ntilde", 241);
      ("ograve", 242);
      ("oacute", 243);
      ("ocirc", 244);
      ("otilde", 245);
      ("ouml", 246);
      ("divide", 247);
      ("oslash", 248);
      ("ugrave", 249);
      ("uacute", 250);
      ("ucirc", 251);
      ("uuml", 252);
      ("yacute", 253);
      ("thorn", 254);
      ("yuml", 255);
      (* Other: *)
      ("fnof", 402);
      ("Alpha", 913);
      ("Beta", 914);
      ("Gamma", 915);
      ("Delta", 916);
      ("Epsilon", 917);
      ("Zeta", 918);
      ("Eta", 919);
      ("Theta", 920);
      ("Iota", 921);
      ("Kappa", 922);
      ("Lambda", 923);
      ("Mu", 924);
      ("Nu", 925);
      ("Xi", 926);
      ("Omicron", 927);
      ("Pi", 928);
      ("Rho", 929);
      ("Sigma", 931);
      ("Tau", 932);
      ("Upsilon", 933);
      ("Phi", 934);
      ("Chi", 935);
      ("Psi", 936);
      ("Omega", 937);
      ("alpha", 945);
      ("beta", 946);
      ("gamma", 947);
      ("delta", 948);
      ("epsilon", 949);
      ("zeta", 950);
      ("eta", 951);
      ("theta", 952);
      ("iota", 953);
      ("kappa", 954);
      ("lambda", 955);
      ("mu", 956);
      ("nu", 957);
      ("xi", 958);
      ("omicron", 959);
      ("pi", 960);
      ("rho", 961);
      ("sigmaf", 962);
      ("sigma", 963);
      ("tau", 964);
      ("upsilon", 965);
      ("phi", 966);
      ("chi", 967);
      ("psi", 968);
      ("omega", 969);
      ("thetasym", 977);
      ("upsih", 978);
      ("piv", 982);
      ("bull", 8226);
      ("hellip", 8230);
      ("prime", 8242);
      ("Prime", 8243);
      ("oline", 8254);
      ("frasl", 8260);
      ("weierp", 8472);
      ("image", 8465);
      ("real", 8476);
      ("trade", 8482);
      ("alefsym", 8501);
      ("larr", 8592);
      ("uarr", 8593);
      ("rarr", 8594);
      ("darr", 8595);
      ("harr", 8596);
      ("crarr", 8629);
      ("lArr", 8656);
      ("uArr", 8657);
      ("rArr", 8658);
      ("dArr", 8659);
      ("hArr", 8660);
      ("forall", 8704);
      ("part", 8706);
      ("exist", 8707);
      ("empty", 8709);
      ("nabla", 8711);
      ("isin", 8712);
      ("notin", 8713);
      ("ni", 8715);
      ("prod", 8719);
      ("sum", 8721);
      ("minus", 8722);
      ("lowast", 8727);
      ("radic", 8730);
      ("prop", 8733);
      ("infin", 8734);
      ("ang", 8736);
      ("and", 8743);
      ("or", 8744);
      ("cap", 8745);
      ("cup", 8746);
      ("int", 8747);
      ("there4", 8756);
      ("sim", 8764);
      ("cong", 8773);
      ("asymp", 8776);
      ("ne", 8800);
      ("equiv", 8801);
      ("le", 8804);
      ("ge", 8805);
      ("sub", 8834);
      ("sup", 8835);
      ("nsub", 8836);
      ("sube", 8838);
      ("supe", 8839);
      ("oplus", 8853);
      ("otimes", 8855);
      ("perp", 8869);
      ("sdot", 8901);
      ("lceil", 8968);
      ("rceil", 8969);
      ("lfloor", 8970);
      ("rfloor", 8971);
      ("lang", 9001);
      ("rang", 9002);
      ("loz", 9674);
      ("spades", 9824);
      ("clubs", 9827);
      ("hearts", 9829);
      ("diams", 9830);
      ("OElig", 338);
      ("oelig", 339);
      ("Scaron", 352);
      ("scaron", 353);
      ("Yuml", 376);
      ("circ", 710);
      ("tilde", 732);
      ("ensp", 8194);
      ("emsp", 8195);
      ("thinsp", 8201);
      ("zwnj", 8204);
      ("zwj", 8205);
      ("lrm", 8206);
      ("rlm", 8207);
      ("ndash", 8211);
      ("mdash", 8212);
      ("lsquo", 8216);
      ("rsquo", 8217);
      ("sbquo", 8218);
      ("ldquo", 8220);
      ("rdquo", 8221);
      ("bdquo", 8222);
      ("dagger", 8224);
      ("Dagger", 8225);
      ("permil", 8240);
      ("lsaquo", 8249);
      ("rsaquo", 8250);
      ("euro", 8364);
    ]

  let quick_etable_html =
    let ht = Hashtbl.create 50 in
    List.iter (fun (name, value) -> Hashtbl.add ht name value) etable;
    ht

  let quick_etable_xml =
    let ht = Hashtbl.create 5 in
    List.iter
      (fun name ->
        let value = List.assoc name etable in
        Hashtbl.add ht name value)
      [ "lt"; "gt"; "amp"; "quot"; "apos" ];
    ht

  let rev_etable =
    (* Only code points 0 to 255: *)
    let a = Array.make 256 "" in
    List.iter
      (fun (name, value) -> if value <= 255 then a.(value) <- "&" ^ name ^ ";")
      etable;
    a

  let rev_etable_rest =
    (* Only code points >= 256: *)
    let ht = Hashtbl.create 150 in
    List.iter
      (fun (name, value) ->
        if value >= 256 then Hashtbl.add ht value ("&" ^ name ^ ";"))
      etable;
    ht

  let unsafe_chars_html4 =
    "<>\"&\000\001\002\003\004\005\006\007\008\011\012\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031\127"

  let encode_poly ~in_enc ~in_ops ~out_kind ?(out_enc = `Enc_utf8)
      ?(prefer_name = true) ?(unsafe_chars = unsafe_chars_html4) () =
    (* This function implements the general case *)
    for i = 0 to String.length unsafe_chars - 1 do
      if Char.code unsafe_chars.[i] >= 128 then
        invalid_arg
          "Netencoding.Html.encode: non-ASCII character in unsafe_chars"
    done;
    (* Are there better implementations than the general one? *)
    (* Create the domain function: *)
    let dom_array = Array.make 128 true in
    let dom p = p >= 128 || dom_array.(p) in
    (* Set dom_array from unsafe_chars: *)
    for i = 0 to String.length unsafe_chars - 1 do
      let c = Char.code unsafe_chars.[i] in
      dom_array.(c) <- false
    done;
    (* Create the substitution function: *)
    let subst p =
      let name =
        if prefer_name then
          if p <= 255 then rev_etable.(p)
          else try Hashtbl.find rev_etable_rest p with Not_found -> ""
        else ""
      in
      if name = "" then "&#" ^ string_of_int p ^ ";" else name
    in
    (* Recode: *)
    fun s ->
      Netconversion.convert_poly ~in_ops ~out_kind ~subst ~in_enc
        ~out_enc:(`Enc_subset (out_enc, dom))
        s

  let encode ~in_enc ?out_enc ?prefer_name ?unsafe_chars () =
    let in_ops = Netstring_tstring.string_ops in
    let out_kind = Netstring_tstring.String_kind in
    encode_poly ~in_enc ~in_ops ~out_kind ?out_enc ?prefer_name ?unsafe_chars ()

  type entity_set = [ `Html | `Xml | `Empty ]

  let eref_re =
    Netstring_str.regexp
      "&\\(#\\([0-9]+\\);\\|#[xX]\\([0-9a-fA-F]+\\);\\|\\([a-zA-Z]+\\);\\)"

  let total_enc =
    (* every byte must have a corresponding Unicode code point, i.e. the
     * encoding must be "byte-total"
     *)
    function
    | _ -> false

  let hex_digit_of_char c =
    match c with
    | '0' .. '9' -> Char.code c - 48
    | 'A' .. 'F' -> Char.code c - 55
    | 'a' .. 'f' -> Char.code c - 87
    | _ -> assert false

  let hex_of_string s =
    let n = ref 0 in
    for i = 0 to String.length s - 1 do
      let d = hex_digit_of_char s.[i] in
      n := (!n lsl 4) lor d
    done;
    !n

  let search_all re s pos =
    let rec search p acc =
      match
        try Some (Netstring_str.search_forward re s p) with Not_found -> None
      with
      | Some (k, r) -> search (k + 1) ((k, r) :: acc)
      | None -> List.rev acc
    in
    search pos []

  let decode_half_poly ~in_enc ~out_kind ~out_enc
      ?(lookup =
        fun name ->
          failwith ("Netencoding.Html.decode: Unknown entity `" ^ name ^ "'"))
      ?(subst =
        fun p ->
          failwith
            ("Netencoding.Html.decode: Character cannot be represented: "
           ^ string_of_int p)) ?(entity_base = (`Html : entity_set)) () =
    (* makechar: *)
    let raw_makechar = Netconversion.makechar out_enc in
    let makechar p = try raw_makechar p with Not_found -> subst p in
    (* Entity lookup: *)
    let lookup_entity =
      match entity_base with
      | `Html | `Xml -> (
          let ht =
            if entity_base = `Html then quick_etable_html else quick_etable_xml
          in
          fun name ->
            try makechar (Hashtbl.find ht name) with Not_found -> lookup name)
      | `Empty -> lookup
    in
    (* Recode strings: *)
    let recode_str =
      if total_enc in_enc && in_enc = out_enc then fun s pos len ->
        if pos = 0 && len = String.length s then s else String.sub s pos len
      else fun s range_pos range_len ->
        Netconversion.convert ~in_enc ~out_enc ~subst ~range_pos ~range_len s
    in
    fun s ->
      (* Find all occurrences of &name; or &#num; or &#xnum; *)
      let occurrences = search_all eref_re s 0 in
      (* Collect the resulting string in a buffer *)
      let buf = Netbuffer.create 250 in
      let n = ref 0 in
      List.iter
        (fun (n0, r) ->
          let n1 = Netstring_str.match_end r in
          if n0 > !n then Netbuffer.add_string buf (recode_str s !n (n0 - !n));
          let replacement =
            let num =
              try Netstring_str.matched_group r 2 s with Not_found -> ""
            in
            (* Note: Older versions of Pcre return "" when the substring
               * did not match, newer versions raise Not_found
            *)
            if num <> "" then
              let n = int_of_string num in
              makechar n
            else
              let xnum =
                try Netstring_str.matched_group r 3 s with Not_found -> ""
              in
              (* Note: Older versions of Pcre return "" when the substring
                 * did not match, newer versions raise Not_found
              *)
              if xnum <> "" then
                let n = hex_of_string xnum in
                makechar n
              else
                let name =
                  try Netstring_str.matched_group r 4 s with Not_found -> ""
                in
                (* Note: Older versions of Pcre return "" when the substring
                        * did not match, newer versions raise Not_found
                *)
                assert (name <> "");
                lookup_entity name
          in
          Netbuffer.add_string buf replacement;
          n := n1)
        occurrences;
      let n0 = String.length s in
      if n0 > !n then Netbuffer.add_string buf (recode_str s !n (n0 - !n));
      (* Return *)
      Netbuffer.to_tstring_poly buf out_kind

  let decode ~in_enc ~out_enc ?lookup ?subst ?entity_base () =
    let out_kind = Netstring_tstring.String_kind in
    decode_half_poly ~in_enc ~out_kind ~out_enc ?lookup ?subst ?entity_base ()
end
