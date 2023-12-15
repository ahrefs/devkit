open ExtLib

let explode s =
  let l = String.length s in
  let rec loop k = if k < l then s.[k] :: loop (k + 1) else [] in
  loop 0

let implode l =
  let n = List.length l in
  let s = Bytes.create n in
  let k = ref 0 in
  List.iter
    (fun c ->
      Bytes.set s !k c;
      incr k)
    l;
  Bytes.to_string s

let quote_set s =
  let l = explode s in
  let have_circum = List.mem '^' l in
  let have_minus = List.mem '-' l in
  let have_rbracket = List.mem ']' l in
  let l1 = List.filter (fun c -> c <> '^' && c <> '-' && c <> ']') l in
  let l2 = if have_rbracket then ']' :: l1 else l1 in
  let l3 = if have_circum then l2 @ [ '^' ] else l2 in
  let l4 = if have_minus then l3 @ [ '-' ] else l3 in
  let s4 = implode l4 in
  match s4 with
  | "" -> failwith "Netstring_str.quote_set: empty"
  | "^" -> "^"
  | "^-" -> "[-^]"
  | _ -> "[" ^ s4 ^ "]"

type setatom = Schar of char | Srange of (char * char)
and set = setatom list

type re_term =
  | Texact of string (* literal characters (except NUL) *)
  | Tnullchar (* NUL characer *)
  | Tany (* . but no newline *)
  | Tnull (* emptiness *)
  | Tconcat of re_term list
  | Tstar of re_term (* x* *)
  | Tplus of re_term (* x+ *)
  | Toption of re_term (* x? *)
  | Tset of set (* [...] *)
  | Tnegset of set (* [^...] *)
  | Tbegline (* ^ *)
  | Tendline (* $ *)
  | Talt of re_term list (* x\|y *)
  | Tgroup of (int * re_term) (* \(...\) *)
  | Trefer of int (* \i *)
  | Twordbound (* \b *)

(**********************************************************************)
(* Final types *)

type regexp = Pcre.regexp
type split_result = Str.split_result = Text of string | Delim of string
type result = Pcre.substrings

(**********************************************************************)
(* Parse Str-style regexps, and convert to Pcre-style regexps *)

let scan_str_regexp re_string =
  let l = String.length re_string in
  let k = ref (-1) in
  let c = ref ' ' in
  let esc = ref false in
  let group = ref 1 in
  let n_open_groups = ref 0 in
  let closed_groups = Array.create 10 false in

  let next () =
    incr k;
    if !k < l then
      let c1 = re_string.[!k] in
      if c1 = '\\' then
        if !k < l then (
          incr k;
          c := re_string.[!k];
          esc := true)
        else failwith "Web.Url.Netstring_str regexp: bad backslash"
      else (
        esc := false;
        c := c1)
  in

  let next_noesc () =
    incr k;
    if !k < l then (
      c := re_string.[!k];
      esc := false)
  in

  let rec scan_alternative () =
    let t1 = scan_concatenation () in
    if !k < l then
      if !esc && !c = '|' then (
        next ();
        match scan_alternative () with
        | Talt alist -> Talt (t1 :: alist)
        | t -> Talt [ t1; t ])
      else t1
    else t1
  and scan_concatenation () =
    let t1 = scan_repetition () in
    if t1 = Tnull then t1
    else
      let t2 = scan_concatenation () in
      match t2 with
      | Tnull -> t1
      | Texact s2 -> (
          match t1 with
          | Texact s1 -> Texact (s1 ^ s2)
          | _ -> Tconcat [ t1; t2 ])
      | Tconcat clist -> Tconcat (t1 :: clist)
      | _ -> Tconcat [ t1; t2 ]
  and scan_repetition () =
    let t1 = ref (scan_literal_or_group ()) in
    let continue = ref true in
    while !continue do
      if !k < l && not !esc then
        match !c with
        | '*' ->
            next ();
            t1 := Tstar !t1
        | '+' ->
            next ();
            t1 := Tplus !t1
        | '?' ->
            next ();
            t1 := Toption !t1
        (* {...} is not implemented in Str *)
        | _ -> continue := false
      else continue := false
    done;
    !t1
  and scan_literal_or_group () =
    if !k >= l then Tnull
    else if !esc then (
      match !c with
      | '(' ->
          next ();
          let n = !group in
          incr group;
          incr n_open_groups;
          let t = scan_alternative () in
          decr n_open_groups;
          if !k < l && !esc && !c = ')' then (
            next ();
            closed_groups.(n) <- true;
            Tgroup (n, t))
          else failwith "regexp: closing paranthesis \\) not found"
      | '1' .. '9' ->
          let n = Char.code !c - Char.code '0' in
          if closed_groups.(n) then (
            next ();
            Trefer n)
          else failwith "regexp: bad reference to group"
      (*
      |	'w' -> next(); Twordchar
      |	'W' -> next(); Tnowordchar
      *)
      | 'b' ->
          next ();
          Twordbound
      (*
      |	'B' -> next(); Tnowordbound
      |	'<' -> next(); Twordbeg
      |	'>' -> next(); Twordend
      |	'`' -> next(); Tbegbuf
      |	'\'' -> next(); Tendbuf
      *)
      | '\\' ->
          next ();
          Texact (String.make 1 '\\')
      | '|' -> Tnull
      | ')' ->
          if !n_open_groups > 0 then Tnull
          else failwith "regexp: unmatched closing parenthesis"
      | ch ->
          next ();
          Texact (String.make 1 ch))
    else
      match !c with
      | '*' -> Tnull
      | '+' -> Tnull
      | '?' -> Tnull
      | '{' -> Tnull
      | '^' ->
          next ();
          Tbegline
      | '$' ->
          next ();
          Tendline
      | '.' ->
          next ();
          Tany
      | '\000' ->
          next ();
          Tnullchar
      | '[' ->
          next_noesc ();
          if !k < l then (
            let negated = ref false in
            let set = ref [] in

            let add_char c = set := Schar c :: !set in

            let add_range c1 c2 = set := Srange (c1, c2) :: !set in

            if !c = '^' then (
              next_noesc ();
              negated := true);

            let continue = ref true in
            let first = ref true in

            (* the character after [ or [^ ? *)
            while !continue && !k < l do
              match () with
              | () when !c = '[' && !k + 1 < l && re_string.[!k + 1] = ':' ->
                  failwith
                    "regexp: Character classes such as [[:digit:]] not \
                     implemented"
              (* TODO: check for predefined sets *)
              | () when !c = ']' && not !first ->
                  next ();
                  continue := false
              | ()
                when !k + 2 < l
                     && re_string.[!k + 1] = '-'
                     && re_string.[!k + 2] <> ']' ->
                  (* range *)
                  add_range !c re_string.[!k + 2];
                  next_noesc ();
                  next_noesc ();
                  next_noesc ();
                  first := false
              | () ->
                  add_char !c;
                  next_noesc ();
                  first := false
            done;

            if !continue then failwith "regexp: closing bracket ] not found";

            if !negated then Tnegset !set else Tset !set)
          else failwith "regexp: closing bracket ] not found"
      | ch ->
          next ();
          Texact (String.make 1 ch)
  in

  try
    next ();
    scan_alternative ()
  with Failure msg -> failwith (msg ^ " - regexp: " ^ re_string)

let pcre_safe_quote c =
  (* for print_set *)
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> String.make 1 c
  | '\000' -> "\\000"
  | _ -> "\\" ^ String.make 1 c

let rec print_pcre_regexp ret =
  match ret with
  | Texact s -> Pcre.quote s
  | Tnullchar ->
      (* Pcre.quote "\000" returns nonsense *)
      "[\\000]"
  | Tany -> "."
  | Tnull -> "(?:)"
  | Tconcat l -> String.concat "" (List.map print_pcre_regexp l)
  | Tstar ret' -> print_pcre_subregexp ret' ^ "*"
  | Tplus ret' -> print_pcre_subregexp ret' ^ "+"
  | Toption ret' -> print_pcre_subregexp ret' ^ "?"
  | Tset s -> "[" ^ print_set s ^ "]"
  | Tnegset s -> "[^" ^ print_set s ^ "]"
  | Talt l -> String.concat "|" (List.map print_pcre_subregexp l)
  | Tgroup (_, ret') -> "(" ^ print_pcre_regexp ret' ^ ")"
  | Trefer n ->
      (* Put parentheses around \n to disambiguate from \nn *)
      "(?:\\" ^ string_of_int n ^ ")"
  | Tbegline -> "^"
  | Tendline -> "(?:$)"
  | Twordbound -> "\\b"

and print_pcre_subregexp ret =
  (* Print ret, but put parentheses around ret *)
  match ret with
  | Tset _ | Tnegset _ | Tgroup (_, _) ->
      (* No additional parentheses needed *)
      print_pcre_regexp ret
  | _ ->
      (* Print (?:ret). This is the "neutral" form of grouping that only
         * changes precedence
      *)
      "(?:" ^ print_pcre_regexp ret ^ ")"

and print_set s =
  String.concat ""
    (List.map
       (function
         | Schar c -> pcre_safe_quote c
         | Srange (c1, c2) -> pcre_safe_quote c1 ^ "-" ^ pcre_safe_quote c2)
       s)

(**********************************************************************)
(* Emulation *)

let regexp s =
  let ret = scan_str_regexp s in
  let s' = print_pcre_regexp ret in
  Pcre.regexp ~flags:[ `MULTILINE ] s'

let search_forward pat s pos =
  let result = Pcre.exec ~rex:pat ~pos s in
  (fst (Pcre.get_substring_ofs result 0), result)

let matched_string result _ =
  (* Unfortunately, Pcre.get_substring will not raise Not_found if there is
   * no matched string. Instead, it returns "", but this value cannot be
   * distinguished from an empty match.
   * The workaround is to call Pcre.get_substring_ofs first. This function
   * will raise Not_found if there is not any matched string.
   *
   * NOTE: Current versions of Pcre do return Not_found!
   *)
  ignore (Pcre.get_substring_ofs result 0);
  Pcre.get_substring result 0

let match_beginning result = fst (Pcre.get_substring_ofs result 0)
let match_end result = snd (Pcre.get_substring_ofs result 0)

let matched_group result n _ =
  (* See also the comment for [matched_string] *)
  if n < 0 || n >= Pcre.num_of_subs result then raise Not_found;
  ignore (Pcre.get_substring_ofs result n);
  Pcre.get_substring result n

let global_substitute pat subst s =
  Pcre.substitute_substrings ~rex:pat ~subst:(fun r -> subst r s) s

let tr_split_result r =
  List.map
    (function
      | Pcre.Text t -> Text t | Pcre.Delim d -> Delim d | _ -> assert false)
    (List.filter
       (function Pcre.Group (_, _) | Pcre.NoGroup -> false | _ -> true)
       r)

let full_split sep s = tr_split_result (Pcre.full_split ~rex:sep ~max:(-1) s)
