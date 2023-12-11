(** web utilities *)

open ExtLib
open Printf
open Prelude
open Control

let log = Log.self

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
end = struct
  module Netstring_str : sig
    type regexp
    (** The type of regular expressions *)

    type split_result = Str.split_result =
      | Text of string
      | Delim of string  (** Here we keep compatibility with [Str] *)

    type result
    (** The type of matching results *)

    val regexp : string -> regexp
    (** Parses a regexp *)

    val matched_string : result -> string -> string
    (** Extracts the matched part from the string. The string argument
    * must be the same string passed to [string_match] or the search
    * functions, and the result argument must be the corresponding
    * result.
    *)

    val match_beginning : result -> int
    (** Returns the position where the matched part begins *)

    val full_split : regexp -> string -> split_result list
    (** Like [split_delim], but returns the delimiters in the result *)

    val global_substitute :
      regexp -> (result -> string -> string) -> string -> string
    (** [global_substitute re subst s]: Applies the substitution function
    * [subst] to all matchings of [re] in [s], and returns the 
    * transformed string. [subst] is called with the current [result]
    * of the match and the whole string [s].
    *)
  end = struct
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
                  | () when !c = '[' && !k + 1 < l && re_string.[!k + 1] = ':'
                    ->
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

    let global_substitute pat subst s =
      Pcre.substitute_substrings ~rex:pat ~subst:(fun r -> subst r s) s

    let tr_split_result r =
      List.map
        (function
          | Pcre.Text t -> Text t | Pcre.Delim d -> Delim d | _ -> assert false)
        (List.filter
           (function Pcre.Group (_, _) | Pcre.NoGroup -> false | _ -> true)
           r)

    let full_split sep s =
      tr_split_result (Pcre.full_split ~rex:sep ~max:(-1) s)

  end

  (* copied from https://gitlab.com/gerdstolpmann/lib-ocamlnet3/-/blob/4d1a8401bd40c17632128545e2aa4c880535e208/code/src/netstring/netencoding.ml#L993 *)
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

(** percent-encode (convert space into %20) *)
let rawurlencode = Url.encode ~plus:false

(** percent-encode, but convert space into plus, not %20 *)
let urlencode = Url.encode ~plus:true

(** percent-decode (leave plus as is) *)
let rawurldecode s = try Url.decode ~plus:false s with _ -> s

(** percent-decode and convert plus into space *)
let urldecode s = try Url.decode ~plus:true s with _ -> s

let htmlencode = Netencoding.Html.encode ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 ()
let htmldecode_exn = Netencoding.Html.decode ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 ()
let htmldecode =
  (* U+FFFD REPLACEMENT CHARACTER *)
  let u_FFFD = "\xEF\xBF\xBD" in
  let subst _ = u_FFFD in
  let lookup _ = u_FFFD in
  Netencoding.Html.decode ~subst ~lookup ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 ()
let htmldecode_relaxed = Netencoding.Html.decode ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 ~lookup:(sprintf "&%s;") ()

(* TODO uncomment when httpev becomes less strict everywhere *)
let make_url_args = String.concat "&" $ List.map (function (* (k, "") -> urlencode k | *) (k,v) -> urlencode k ^ "=" ^ urlencode v)

(** Minimum strictness, Neturl will fail on malformed parameters in url *)
let parse_url_args args =
  Stre.nsplitc_rev args '&' |>
  List.rev_map (fun s -> Stre.dividec s '=' |> apply2 urldecode)

let url_get_args url = try String.split url "?" |> snd |> parse_url_args with _ -> []

let () = Curl.global_init Curl.CURLINIT_GLOBALALL

let curl_times h =
  Curl.[
    "Curl.dns", get_namelookuptime h;
    "Curl.conn", get_connecttime h;
(*       "Curl.app", get_appconnecttime; *)
    "Curl.pre", get_pretransfertime h;
    "Curl.start", get_starttransfertime h;
    "Curl.total", get_totaltime h;
  ]

module CurlCache = Cache.Reuse(struct type t = Curl.t let create = Curl.init let reset = Curl.reset end)

let curl_default_setup h =
  Curl.set_nosignal h true;
  Curl.set_connecttimeout h 30;
  Curl.set_timeout h 60;
  Curl.set_followlocation h false;
  Curl.set_encoding h Curl.CURL_ENCODING_ANY;
  ()

type http_action_old =
[ `GET
| `POST_FORM of (string * string) list
| `POST of (string * string) (* content-type and body *)
| `PUT of (string * string)
| `DELETE
| `CUSTOM of (string * string * string) (* request, content-type and body *)
]

type http_body =
[ `Raw of string * string (* content-type and body *)
| `Form of (string * string) list (* key value *)
| `Chunked of (string * (unit -> string)) (* content-type and body readfunction *)
]

type http_action =
[ `GET
| `POST
| `PUT
| `PATCH
| `DELETE
| `CUSTOM of string
]

let string_of_http_action : http_action -> string = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `PATCH -> "PATCH"
  | `DELETE -> "DELETE"
  | `CUSTOM s -> s

let http_action_of_string : string -> http_action = function
  | "GET" -> `GET
  | "POST" -> `POST
  | "PUT" -> `PUT
  | "PATCH" -> `PATCH
  | "DELETE" -> `DELETE
  | s -> Exn.fail "http_action_of_string %S" s

module type IO_TYPE = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val bracket : 'a t -> ('a -> unit t) -> ('a -> 'b t) -> 'b t
  val sleep : Time.t -> unit t
  val fail : ?exn:exn -> ('a, unit, string, 'b t) format4 -> 'a
  val raise : exn -> 'a t
  val map_s : ('a -> 'b t) -> 'a list -> 'b list t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
end

module type CURL = sig
  type 'a t
  val perform : Curl.t -> Curl.curlCode t
end

type ('body,'ret) http_request_ =
  ?ua:string ->
  ?timeout:int ->
  ?verbose:bool ->
  ?setup:(Curl.t -> unit) ->
  ?timer:Action.timer ->
  ?max_size:int ->
  ?http_1_0:bool ->
  ?headers:string list ->
  ?body:'body ->
  http_action -> string -> 'ret

type 'ret http_request = ([ `Form of (string * string) list | `Raw of string * string ], 'ret)  http_request_

module type HTTP = sig
  module IO : IO_TYPE
  val with_curl : (Curl.t -> 'a IO.t) -> 'a IO.t
  val with_curl_cache : (Curl.t -> 'a IO.t) -> 'a IO.t

  type ('body,'ret) request_ = ('body,'ret IO.t) http_request_
  type 'ret request = 'ret IO.t http_request

  val http_request' : [> `Error of Curl.curlCode | `Ok of int * string ] request
  val http_request :  [> `Error of string | `Ok of string ] request
  val http_request_exn : string request
  val http_query : (string * string, [> `Error of string | `Ok of string ]) request_
  val http_submit :
    ?ua:string ->
    ?timeout:int ->
    ?verbose:bool ->
    ?setup:(Curl.t -> unit) ->
    ?timer:Action.timer ->
    ?http_1_0:bool ->
    ?headers:string list ->
    ?action:http_action ->
    string ->
    (string * string) list -> [> `Error of string | `Ok of string ] IO.t
end

let show_result ?(verbose=false) = function
| `Error code -> sprintf "(%d) %s" (Curl.errno code) (Curl.strerror code)
| `Ok (n, content) -> sprintf "http %d%s" n (if verbose then ": " ^ content else "")

let simple_result ?(is_ok=(fun code -> code / 100 = 2)) ?verbose = function
| `Ok (code, s) when is_ok code -> `Ok s
| r -> `Error (show_result ?verbose r)

let nr_http = ref 0

module Http (IO : IO_TYPE) (Curl_IO : CURL with type 'a t = 'a IO.t) : HTTP with type 'a IO.t = 'a IO.t = struct

  module IO = IO

  type ('body,'ret) request_ = ('body,'ret IO.t) http_request_
  type 'ret request = 'ret IO.t http_request

  open IO

  let return_unit = return ()

  let with_curl f = bracket (return @@ Curl.init ()) (fun h -> Curl.cleanup h; return_unit) f
  let with_curl_cache f = bracket (return @@ CurlCache.get ()) (fun h -> CurlCache.release h; return_unit) f

  let update_timer h timer =
    match timer with
    | None -> ()
    | Some t ->
      let total = Curl.get_totaltime h in
      let now = Time.now () in
      t#record "Curl.start" (now -. total);
      curl_times h |> List.iter (fun (name,time) -> t#record name (now -. total +. time));
      ()

  (* deprecated *)
  let http_gets ?(setup=ignore) ?timer ?max_size ?(check=(fun _ -> true)) ?(result=(fun _ _ -> return_unit)) url =
    with_curl_cache begin fun h ->
      Curl.set_url h url;
      curl_default_setup h;
      let () = setup h in
      let b = Buffer.create 10 in
      let read_size = ref 0 in
      Curl.set_writefunction h begin fun s ->
        match check h with
        | false -> 0
        | true ->
          Buffer.add_string b s;
          let l = String.length s in
          read_size += l;
          match max_size with
          | Some max_size when !read_size > max_size -> Exn.fail "received too much data (%db) when max is %db" !read_size max_size
          | _ -> l
      end;
      timer |> Option.may (fun t -> t#mark "Web.http");
      catch (fun () -> Curl_IO.perform h) (fun exn -> update_timer h timer; IO.raise exn) >>= fun code ->
      (update_timer h timer; result h code) >>= fun () ->
      return @@ match code with
      | Curl.CURLE_OK -> `Ok (Curl.get_httpcode h, Buffer.contents b)
      | code -> `Error code
    end

  let verbose_curl_result nr_http action t h code =
    let open Curl in
    let b = Buffer.create 10 in
    bprintf b "%s #%d %s ⇓%s ⇑%s %s "
      (string_of_http_action action) nr_http (Time.compact_duration t#get)
      (Action.bytes_string_f @@ get_sizedownload h)
      (Action.bytes_string_f @@ get_sizeupload h)
      (get_primaryip h)
    ;
    begin match code with
    | CURLE_OK ->
      bprintf b "HTTP %d %s" (get_httpcode h) (get_effectiveurl h);
      begin match get_redirecturl h with
      | "" -> ()
      | s -> bprintf b " -> %s" s
      end;
      begin match get_redirectcount h with
      | 0 -> ()
      | n -> bprintf b " after %d redirects" n
      end
    | _ ->
      bprintf b "error (%d) %s (errno %d)" (errno code) (strerror code) (Curl.get_oserrno h)
    end;
    log #info_s (Buffer.contents b);
    return ()

  (* NOTE don't forget to set http_1_0=true when sending requests to a Httpev-based server *)
  (* Don't use curl_setheaders when using ?headers option *)
  let http_request' ?ua ?timeout ?(verbose=false) ?(setup=ignore) ?timer ?max_size ?(http_1_0=false) ?headers ?body (action:http_action) url =
    let open Curl in
    let set_body_and_headers h ct body =
      set_httpheader h (("Content-Type: "^ct) :: Option.default [] headers);
      set_postfields h body;
      set_postfieldsize h (String.length body)
    in
    let setup h =
      begin match body with
      | Some (`Form args) -> set_body_and_headers h "application/x-www-form-urlencoded" (make_url_args args)
      | Some (`Raw (ct,body)) -> set_body_and_headers h ct body
      | Some (`Chunked (ct,f)) ->
        set_httpheader h (("Content-Type: " ^ ct) :: "Transfer-Encoding: chunked" :: Option.default [] headers);
        set_readfunction h f
      | None ->
        Option.may (set_httpheader h) headers;
        (* prevent reading from stdin with POST without body *)
        set_readfunction h (fun _ -> "");
        (* prevent libcurl 7.66.0+ from sending Transfer-Encoding: chunked for POST without body.
           See https://github.com/curl/curl/pull/4138. *)
        set_postfieldsize h 0
      end;
      begin match action with
      | `GET | `DELETE | `CUSTOM _ -> ()
      | `POST | `PUT | `PATCH -> set_post h true
      end;
      set_customrequest h (string_of_http_action action);
      if http_1_0 then set_httpversion h HTTP_VERSION_1_0;
      Option.may (set_timeout h) timeout;
      Option.may (set_useragent h) ua;
      let () = setup h in
      ()
    in
    let nr_http = incr nr_http; !nr_http in (* XXX atomic wrt ocaml threads *)
    if verbose then begin
      let action = string_of_http_action action in
      let body = match body with
      | None -> ""
      | Some (`Form args) -> String.concat " " @@ List.map (fun (k,v) -> sprintf "%s=\"%s\"" k (Stre.shorten ~escape:true 64 v)) args
      | Some (`Raw (ct,body)) -> sprintf "%s \"%s\"" ct (Stre.shorten ~escape:true 64 body)
      | Some (`Chunked (ct,_f)) -> sprintf "%s chunked" ct
      in
      log #info "%s #%d %s %s" action nr_http url body
    end;
    let t = new Action.timer in
    let result = if verbose then Some (verbose_curl_result nr_http action t) else None in
    http_gets ~setup ?timer ?result ?max_size url

  let http_request ?ua ?timeout ?verbose ?setup ?timer ?max_size ?http_1_0 ?headers ?body (action:http_action) url =
    http_request' ?ua ?timeout ?verbose ?setup ?timer ?max_size ?http_1_0 ?headers ?body action url >>= fun res ->
    return @@ simple_result ?verbose res

  let http_request_exn ?ua ?timeout ?verbose ?setup ?timer ?max_size ?http_1_0 ?headers ?body (action:http_action) url =
    http_request ?ua ?timeout ?verbose ?setup ?timer ?max_size ?http_1_0 ?headers ?body action url
    >>= function `Ok s -> return s | `Error error -> fail "%s" error

  let http_query ?ua ?timeout ?verbose ?setup ?timer ?max_size ?http_1_0 ?headers ?body (action:http_action) url =
    let body = match body with Some (ct,s) -> Some (`Raw (ct,s)) | None -> None in
    http_request ?ua ?timeout ?verbose ?setup ?timer ?max_size ?http_1_0 ?headers ?body action url

  let http_submit ?ua ?timeout ?verbose ?setup ?timer ?http_1_0 ?headers ?(action=`POST) url args =
    http_request ?ua ?timeout ?verbose ?setup ?timer ?http_1_0 ?headers ~body:(`Form args) action url

end

module IO_blocking = struct
  type 'a t = 'a
  let return = identity
  let ( >>= ) m f = f m
  let bracket = bracket
  let fail = Exn.fail
  let raise = raise
  let sleep = Nix.sleep
  let map_s = List.map
  let catch = fun f e -> try f ()  with exn -> e exn
end

module IO_lwt = struct
  type 'a t = 'a Lwt.t
  let return = Lwt.return
  let ( >>= ) = Lwt.( >>= )
  let bracket mresource destroy k =
    let%lwt resource = mresource in
    (k resource) [%finally destroy resource]
  let fail = Exn_lwt.fail
  let raise = Lwt.fail
  let sleep = Lwt_unix.sleep
  let map_s = Lwt_list.map_s
  let catch = Lwt.catch
end


module Curl_blocking = struct
  type 'a t = 'a
  let perform h = try Curl.perform h; Curl.CURLE_OK with Curl.CurlException (code,_,_) -> code
end

module Curl_lwt_for_http = struct
  type 'a t = 'a Lwt.t
  include Curl_lwt
end

module Http_blocking = Http(IO_blocking)(Curl_blocking)
module Http_lwt = Http(IO_lwt)(Curl_lwt_for_http)

let with_curl = Http_blocking.with_curl
let with_curl_cache = Http_blocking.with_curl_cache
let http_request' = Http_blocking.http_request'
let http_request = Http_blocking.http_request
let http_request_exn = Http_blocking.http_request_exn
let http_query = Http_blocking.http_query
let http_submit = Http_blocking.http_submit

let http_request_lwt' = Http_lwt.http_request'
let http_request_lwt = Http_lwt.http_request
let http_request_lwt_exn = Http_lwt.http_request_exn
let http_query_lwt = Http_lwt.http_query
let http_submit_lwt = Http_lwt.http_submit

(* http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html *)
let string_of_http_code = function
| 100 -> "Continue"
| 101 -> "Switching protocols"

| 200 -> "OK"
| 201 -> "Created"
| 202 -> "Accepted"
| 203 -> "Non-Authoritative Information"
| 204 -> "No Content"
| 205 -> "Reset Content"
| 206 -> "Partial Content"

| 300 -> "Multiple Choices"
| 301 -> "Moved Permanently"
| 302 -> "Found"
| 303 -> "See Other"
| 304 -> "Not Modified"
| 305 -> "Use Proxy"
| 306 -> "(Unused)"
| 307 -> "Temporary Redirect"

| 400 -> "Bad Request"
| 401 -> "Unauthorized"
| 402 -> "Payment Required"
| 403 -> "Forbidden"
| 404 -> "Not Found"
| 405 -> "Method Not Allowed"
| 406 -> "Not Acceptable"
| 407 -> "Proxy Authentication Required"
| 408 -> "Request Timeout"
| 409 -> "Conflict"
| 410 -> "Gone"
| 411 -> "Length Required"
| 412 -> "Precondition Failed"
| 413 -> "Request Entity Too Large"
| 414 -> "Request-URI Too Long"
| 415 -> "Unsupported Media Type"
| 416 -> "Requested Range Not Satisfiable"
| 417 -> "Expectation Failed"

| 500 -> "Internal Server Error"
| 501 -> "Not Implemented"
| 502 -> "Bad Gateway"
| 503 -> "Service Unavailable"
| 504 -> "Gateway Timeout"
| 505 -> "HTTP Version Not Supported"

| _ -> "(Unknown)"

let class_of_http_code code =
  match code / 100 with
  | 1 -> "Informational"
  | 2 -> "Successful"
  | 3 -> "Redirection"
  | 4 -> "Client Error"
  | 5 -> "Server Error"
  | n -> sprintf "%dxx" n
