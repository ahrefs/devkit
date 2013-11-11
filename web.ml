(** web utilities *)

open ExtLib
open Printf

open Prelude
open Control

let log = Log.self

let get_host = String.lowercase $ Neturl.url_host $ Neturl.parse_url
let rawurlencode = Netencoding.Url.encode ~plus:false
let urlencode = Netencoding.Url.encode ~plus:true
let urldecode s = try Netencoding.Url.decode s with _ -> s
let htmlencode = Netencoding.Html.encode ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 ()
let htmldecode = Netencoding.Html.decode ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 ()
let make_url_args = String.concat "&" $ List.map (fun (k, v) -> k ^ "=" ^ urlencode v)

(** Minimum strictness, Neturl will fail on malformed parameters in url *)
let url_get_args url =
  try
    String.split url "?" >> snd >> flip String.nsplit "&" >> 
      List.filter_map (fun s -> try String.split s "=" >> apply2 urldecode >> some with _ -> None)
  with
    _ -> []

let extract_n_number =
  let threshold = 2 in
  let rec start = parser
  | [< ''0'..'9' as c; t >] -> number [c] t
  | [< '_; t >] -> start t
  | [< >] -> Exn.fail "no digits"
  and number acc = parser
  | [< ''0'..'9' as c; t >] -> number (c::acc) t
  | [< '_; t >] -> unsure 1 acc t
  | [< >] -> fin acc
  and unsure depth acc = parser
  | [< ''0'..'9' as c; t >] -> number (c::acc) t
  | [< '_; t >] -> if depth >= threshold then fin acc else unsure (depth+1) acc t
  | [< >] -> fin acc
  and fin acc = int_of_string & String.implode & List.rev acc
  in
  fun n s ->
    let s = Stream.of_string s in
    for i = 1 to n-1 do
      let _ = start s in ()
    done;
    start s

let extract_first_number = extract_n_number 1

module T = struct

module Stream = ExtStream

(** scan stream until predicate is true
  @param limit skip not more than [limit] elements
  @return matching element
  @raise Not_found on stream end *)
let rec stream_get ?(limit=max_int) f = parser
  | [< 'x when f x >] -> x
  | [< '_; t >] -> if limit = 0 then raise Not_found else stream_get ~limit:(limit-1) f t
  | [< >] -> raise Not_found

(** see {!stream_get} *)
let stream_find ?limit f s = ignore (stream_get ?limit f s)

(** scan stream while predicate holds, stop on first non-matching element or stream end *)
let rec stream_skip f = parser
  | [< 'x when f x; t >] -> stream_skip f t
  | [< >] -> ()

let stream_skip_till x = stream_skip ((<>) x)

(** @return next stream element if predicate matches
    @raise Not_found otherwise *)
let stream_get_next f = parser
  | [< 'x when f x >] -> x
  | [< >] -> raise Not_found

(** see {!stream_get_next} *)
let stream_next f s = ignore (stream_get_next f s)

let stream_match_next f s =
  match Stream.peek s with
  | None -> raise Not_found
  | Some x ->
    match f x with
    | Some x -> Stream.junk s; x
    | None -> raise Not_found

(** scan stream while predicate holds @return list of matching elements *)
let stream_extract_while f s =
  let rec loop acc = 
    match Stream.peek s with
    | Some x when f x -> Stream.junk s; loop (x::acc)
    | _ -> List.rev acc
  in
  loop []

(** equivalent to [stream_extract_while ((<>) x)] *)
let stream_extract_till x = stream_extract_while ((<>) x)

(** match multiple conditions in arbitrary order
  a = [|cond_1, h_1; cond_2, h_2; ... |]
  find all elements matching [cond_i] (for all [i]) in arbitrary order
  and execute matching [h_i] on the stream at that point
  @return results returned by [h_i] *)
let stream_find_all ~limit a s =
  let result = Array.map (fun _ -> None) a in
  let rec loop () =
    stream_find ~limit (fun x ->
      try
      for i = 0 to Array.length a - 1 do
        let (cond,h) = a.(i) in
        if result.(i) = None && (cond x:bool) then (result.(i) <- Some (h s); raise Exit)
      done; false with Exit -> true) s;
    if Array.exists Option.is_none result then loop () else Array.map (function None -> assert false | Some x -> x) result
  in
  loop ()

end (* T *)

include T

module Provider = struct

  (** url, smth, title, snippet *)
  type entry = string * string option * string * string 

  (**  @return total estimate, organic entries and ads

  raises exn on error *)
  type extract_full = string -> int * entry array * entry array

  type t = { 
    request : ?num:int -> string -> string; 
    extract : string -> string Enum.t; 
    extract_full : extract_full;
  }

  open HtmlStream

let any_text = function Text _ -> true | _ -> false
let any_tag = function Tag _ -> true | _ -> false

let decode s = htmldecode (Raw.proj s)
let make_text l = decode (make_text l)
let make_url l = String.concat "" ("http://" :: List.map (function HtmlStream.Text s -> decode s | _ -> "") l)

module Google = struct

let get_results ?(debug=false) ~parse_url s' =
  let element s = Option.map_default show_raw "EOS!" (Stream.peek s) in
  let s = parse & Stream.of_string s' in
  let total = ref 0 in
  begin try
    let rec search = parser
    | [< 'x when tag "td" x; t >] -> 
      begin match Stream.peek t with
      | Some x when tag "div" ~a:["id","subform_ctrl"] x -> Stream.junk t; stream_skip any_tag t; make_text & stream_extract_while any_text t
      | _ -> search t
      end
    | [< 'x when tag "div" ~a:["id","resultStats"] x; t >] -> stream_skip any_tag t; make_text & stream_extract_while any_text t
    | [< '_; t >] -> search t
    | [< >] -> raise Not_found
    in
    let h = search s in
    if debug then log #info "extract total %S" h;
    total := extract_first_number h;
  with exn ->
    if String.exists s' "No results found in your selected language" ||
      String.exists s' "did not match any documents"
    then
      total := 0
    else
      if String.exists s' "Your client does not have permission to get URL" then
        Exn.fail "blocked"
      else
        Exn.fail ~exn "results count failed"
  end;
  let acc = ref [] in
  let rec loop () =
    if debug then log #info "loop";
    stream_find (tag "li" ~a:["class","g"]) s;
    if debug then log #info "li found %s" (element s);
    begin try
      stream_find (tag "h3" ~a:["class","r"]) s;
      let get_h3_href () =
        if debug then log #info "h3 found %s" (element s);
        let href = decode & match stream_get_next (tag "a") s with
        | Tag (_,l) -> List.assoc "href" l
        | _ -> assert false in
        if debug then log #info "href %s" href;
        let href = if String.starts_with href "/url?" then 
            try List.assoc "q" (url_get_args href) with exn -> Exn.fail ~exn "url?q="
          else
            href
        in
        href
      in
      let href = get_h3_href () in
      let href = (*trying to find another <h3 class="r"> but not farther then </li>*)
      if String.starts_with href "/" then
      begin
        let rec miniloop () =
          match ExtStream.next s with
          | Close "li" -> Exn.fail "not an absolute url : %s" href;
          | x when tag "h3" ~a:["class","r"] x ->
             let href_candidate = get_h3_href () in
             if String.starts_with href_candidate "/" then miniloop () else href_candidate
          | _ -> miniloop ()
        in
        miniloop ()
      end
      else href
      in
      let href = parse_url href in
      let h = stream_extract_till (Close "h3") s in
      if debug then log #info "/h3 found %s" (element s);
(*
      stream_find (tag "span" ~a:["class","st"]) s;
      log #info "class=st found %s" (Option.map_default show "END" & Stream.peek s);
      let t = stream_extract_till (Close "span") s in
*)
      let rec extract_description () =
        let x = ExtStream.next s in
(*        if tag "span" ~a:["class","f"] x then
        begin
          stream_find (tag "br") s;
          stream_extract_while (not $ tag "br") s
        end
        else *) (* temporary disable *)
        if tag "span" ~a:["class", "st"] x then
        begin
          let nr_span = ref 0 in
          let rec skip () =
          match ExtStream.next s with (* skip date or x days ago messages *)
          | x when tag "span" x -> incr nr_span; skip ()
          | Close "span" when !nr_span = 1 -> ()
          | Close "span" -> decr nr_span; skip ()
          | _ -> skip ()
          in
          begin match Stream.peek s with
          | Some x when tag "span" x -> skip ()
          | _ -> ()
          end;
          stream_extract_while (not $ close "span") s
        end
        else if tag "div" ~a:["class","s"] x then
          match Stream.peek s with
          | Some Close _ | None -> []
          | Some x when tag "div" x -> extract_description ()
          | _ -> stream_extract_while (not $ tag "div") s
        else if tag "span" x then
        begin
          stream_skip (not $ close "span") s;
          extract_description ()
        end
        else
          extract_description ()
      in
      let t = extract_description () in
      if debug then log #info "extracted description : %s" (element s);
      acc := (href,None,make_text h, make_text t) :: !acc;
    with exn -> log #debug ~exn "skipped search result : %s" (element s) end;
    loop ()
  in
  if !total <> 0 then begin try loop () with Not_found -> () end;
  !total, Array.of_list & List.rev !acc, [||]

(*
let get_results ?debug s =
  try Some (get_results ?debug s) with exn -> log #warn ~exn "Google.get_results"; None
*)

exception Next

let get_ads1 ~debug ~parse_url s =
  let s = parse & Stream.of_string s in
  let acc = ref [] in
  let pt name = if debug then log #info "get_ads1 : %s : %s" name (Option.map_default show_raw "END" & Stream.peek s) in
  let one () =
    stream_find (tag "li") s;
    begin try
      let () = match stream_get_next (tag "div") s with
      | Tag (_, ("class",x) :: _) when x = Raw.inj "vsc vsra" || x = Raw.inj "vsc vsta" -> ()
      | x -> Exn.fail "test #1 : %s" (show_raw x)
      in
      pt "div";
      stream_find ~limit:2 (tag "h3") s;
      pt "h3";
      let adurl = match stream_get_next (tag "a") s with
      | Tag (_,l) ->
          List.assoc "href" l >>
          decode >>
          url_get_args >>
          List.assoc "adurl" >>
          parse_url
      | _ -> assert false
      in
      let h = stream_extract_till (Close "a") s in
      stream_find ~limit:10 (tag "cite") s;
      let href = parse_url & make_url & stream_extract_till (Close "cite") s in
      stream_find (tag "span" ~a:["class","ac"]) s;
      let t = stream_extract_till (Close "span") s in
      acc := (href,Some adurl,make_text h,make_text t) :: !acc;
      pt "found";
    with
    | exn -> if debug then log #info ~exn "get_ads1 : skipped ad result : %s" (Option.map_default show_raw "END" & Stream.peek s)
    end
  in
  let rec loop () = one (); loop () in
  begin try loop () with Not_found -> () | exn -> log #warn ~exn "Google.get_ads" end;
  Array.of_list & List.rev !acc

let get_ads2 ~debug ~parse_url s =
  let s = parse & Stream.of_string s in
  let acc = ref [] in
(*   let pt _ = () in *)
  let pt name = if debug then log #info "get_ads2 : %s : %s" name (Option.map_default show_raw "END" & Stream.peek s) in
  let one () =
    stream_find (tag "li") s;
    begin try
      let () = stream_skip (not $ tag "h3") s in
      stream_skip (tag "h3") s;
      pt "h3";
      let adurl = match stream_get_next (tag "a") s with
      | Tag (_,l) ->
          List.assoc "href" l >>
          decode >>
          url_get_args >>
          List.assoc "adurl" >>
          parse_url
      | _ -> assert false
      in
      pt "adurl";
      let h = stream_extract_till (Close "a") s in
      pt "a";
      stream_skip (fun x -> not (tag "cite" ~a:[] x || tag "span" ~a:["class","ac"] x || tag "span" ~a:["class","a"] x)) s;
      match Stream.peek s with
      | Some x when tag "cite" ~a:[] x || tag "span" ~a:["class","ac"] x  ->
      begin
        match stream_find_all ~limit:20
          [| tag "cite" ~a:[], make_url $ stream_extract_till (Close "cite");
             tag "span" ~a:["class","ac"], make_text $ stream_extract_till (Close "span");
          |]
          s
        with
        | [| href; t |] -> 
          pt "found";
          acc := (parse_url href,Some adurl,make_text h,t) :: !acc;
          pt "got ad";
        | _ -> assert false
      end
      | Some Tag ("span",_) ->
      begin
        match stream_find_all ~limit:20
          [| tag "span" ~a:["class","a"], make_text $ stream_extract_till (Close "span"); |]
          s
        with
        | [| t |] ->
          pt "found";
          acc := (adurl, Some adurl, make_text h, t) :: !acc;
          pt "got add";
        | _ -> assert false
      end
      | _ -> assert false
    with
    | exn -> if debug then log #info ~exn "get_ads2 failed : %s" (Option.map_default show_raw "END" & Stream.peek s)
    end
  in
  let rec loop () = one (); loop () in
  begin try loop () with Not_found -> () | exn -> log #warn ~exn "Google.get_ads" end;
  Array.of_list & List.rev !acc

let get_ads ?(debug=false) ~parse_url s =
  (*Array.append (get_ads1 ~debug ~parse_url s)*) (get_ads2 ~debug ~parse_url s)

type params = { tld:string; lang:string; hl:string; gl:string; }

let query p num q =
(* wrong criteria, better parse it around
  (* disable google calculator *)
  let q = if Pcre.pmatch ~rex:rex_digits q then "+"^q else q in
*)
  sprintf "http://www.google.%s/search?nomo=1&hl=%s&gl=%s&pws=0&q=%s&num=%d&as_qdr=all&oe=utf-8" p.tld p.hl p.gl (urlencode q) num

end (* Google *)

  let google p =
    let re = Pcre.regexp ~flags:[`CASELESS] "<h3 class=r><a href=\"([^\"]+)\" class=l" in
    { extract = Stre.enum_extract re;
      request = (fun ?(num=10) q -> Google.query p num q);
      extract_full = (fun s -> Google.get_results ~parse_url:id s);
    }

(*
  let google_day =
    let re = Pcre.regexp ~flags:[`CASELESS] "<h3 class=r><a href=\"([^\"]+)\" class=l" in
    { extract = Stre.enum_extract re;
      request = (fun ?(num=10) q ->
        sprintf "http://www.google.com/search?hl=en&q=%s&num=%u&btnG=Search&aq=f&oq=&aqi=&tbs=qdr:d,sbd:1"
          (urlencode q) num);
      extract_full = google_full;
    }
*)

  let parse_rss s =
    let xml = Xmlm.make_input ~strip:true (`String (0,s)) in
    let rec skip () = 
      match Xmlm.peek xml with
      | `Dtd _ -> ignore & Xmlm.input xml; skip ()
      | _ -> () in
    skip ();
    let link = ref "" and title = ref "" and desc = ref "" in
(*
    let rec show = function
      | `R (l,_,_) -> log #info "result : %s _ _" l
      | `U -> log #info "undef"
      | `L l -> log #info "list"; List.iter (fun (l,_,_) -> log #info "list result %s" l) l; log #info "list end"
      | `D s -> log #info "data %s" s
    in
*)
    match Xmlm.input_tree ~data:(fun s -> `D s) ~el:(fun ((_,name),_) ch ->
      match name,ch with
      | "item", _ -> let r = (!link, None, !title, !desc) in link := ""; title := ""; desc := ""; `R r
      | "link",[`D s] -> link := s; `U
      | "description", [`D s] -> desc := s; `U
      | "title", [`D s] -> title := s; `U
      | "channel", l -> `L (List.filter_map (function (`R x) -> Some x | _ -> None) l)
      | "rss",[x] -> x
      | "rss",l -> 
          log #warn "bad rss (%d)" (List.length l); 
          begin try List.find (function `L _ -> true | _ -> false) l with _ -> `U end
      | _ -> (*log #warn "unrec : %s" s;*) `U) xml
    with
    | `L l -> 0, Array.of_list l, [||]
    | _ -> log #warn "unrecognized result"; 0, [||], [||]

  let bing_html s' =
    let s = parse & Stream.of_string s' in
    let total = ref 0 in
    let is_digit = function '0'..'9' -> true | _ -> false in
    begin try
      stream_find (tag "span" ~a:["class","sb_count"]) s;
      match Stream.next s with
      | Text s ->
        let l = (* either "N results" or "K of N results" *)
        Stre.nsplitc (decode s) ' ' >>
        List.map String.strip >>
        List.filter (fun s -> s <> "") >>
        List.rev >>
        List.dropwhile (fun s -> not (is_digit s.[0]))
        in
        begin match l with
        | [] -> Exn.fail "no digits"
        | s::_ -> total := extract_first_number s
        end
      | _ -> Exn.fail "no text in sb_count"
    with exn ->
      let msg = match exn with Failure s -> s | exn -> Exn.str exn in
      Exn.fail "bad sb_count : %s" msg
    end;
    let res = ref [] in
    let rec loop () =
      stream_find (tag "div" ~a:["class","sb_tlst"]) s;
      begin try
        stream_next (tag "h3") s;
        let href = match stream_get_next (tag "a") s with
        | Tag (_,l) -> List.assoc "href" l >> decode
        | _ -> assert false in
        let h = stream_extract_till (Close "h3") s in
        let _ = stream_skip_till (Tag ("p",[])) s in
        let t = stream_extract_till (Close "p") s in
        res := (href,None,make_text h,make_text t) :: !res;
      with exn -> log #debug ~exn "skipped search result" end;
      loop ()
    in
    begin try loop () with Not_found -> () | exn -> log #warn ~exn "bing_html results" end;
    let s = parse & Stream.of_string s' in

    let ads = ref [] in
    let rec loop () =
      s >> stream_find (fun x ->
        tag "div" ~a:["class","sb_adsW"] x ||
(*         tag "div" ~a:["class","sb_adsW sb_adsW2"] x || *)
        tag "div" ~a:["class","sb_adsN"] x);
      begin try
        while true do
        stream_find ~limit:10 (tag "h3") s;
        let h = stream_extract_till (Close "h3") s in
        Stream.junk s;
        begin try 
          stream_next (tag "span" ~a:["class","sb_adsD"]) s; stream_skip_till (Close "span") s; Stream.junk s;
        with Not_found -> ()
        end;
        let x = Stream.next s in
        let (t,u) = if tag "p" x then
          let t = stream_extract_till (Close "p") s in
          Stream.junk s;
          let () = stream_next (tag "cite") s in
          t, stream_extract_till (Close "cite") s
        else if tag "cite" x then
          let u = stream_extract_till (Close "cite") s in
          Stream.junk s;
          let () = stream_next (tag "p") s in
          stream_extract_till (Close "p") s, u
        else
          Exn.fail "expected <p> or <cite>"
        in
        let href = make_url u in
        ads := (href,None,make_text h,make_text t) :: !ads;
        done
      with exn -> log #debug ~exn "skipped search result" end;
      loop ()
    in
    begin try loop () with Not_found -> () | exn -> log #warn ~exn "bing_html ads" end;
    !total, Array.of_list & List.rev !res, Array.of_list & List.rev !ads

  let bing_html lang =
    let re = Pcre.regexp ~flags:[`CASELESS] "<div class=\"sb_tlst\"><h3><a href=\"([^\"]+)\"" in
    { extract = Stre.enum_extract re;
      request = (fun ?(num=50) q ->
        sprintf "http://www.bing.com/search?q=%s&count=%u%s" 
          (urlencode q) num (match lang with None -> "" | Some s -> "&setmkt=" ^ s));
      extract_full = bing_html;
    }

  let rss_source ~default fmt =
    let re = Pcre.regexp ~flags:[`CASELESS] "<item>.*?<link>([^<]+)</link>.*?</item>" in
    { extract = Stre.enum_extract re;
      request = (fun ?(num=default) q -> sprintf fmt (urlencode q) num);
      extract_full = parse_rss;
    }

  let bing = rss_source ~default:50 "http://www.bing.com/search?q=%s&count=%u&format=rss"
  let google_blogs = rss_source ~default:50 "http://blogsearch.google.com/blogsearch_feeds?q=%s&num=%u&hl=en&safe=off&output=rss"
  let boardreader = rss_source ~default:50 "http://boardreader.com/rss/%s?extended_search=1&s=time_desc&p=%u&format=RSS2.0"

end

let () = Curl.global_init Curl.CURLINIT_GLOBALALL

let with_curl f =
  bracket (Curl.init ()) Curl.cleanup f

let curl_ok h = Curl.get_httpcode h = 200

let curl_default_setup h =
  Curl.set_nosignal h true;
  Curl.set_connecttimeout h 30;
  Curl.set_timeout h 60;
  Curl.set_followlocation h false;
  Curl.set_encoding h Curl.CURL_ENCODING_ANY;
  ()

let http_get_io_exn ?(setup=ignore) ?(check=curl_ok) url out =
  let inner = ref None in
  try
    with_curl begin fun h ->
      Curl.set_url h url;
      curl_default_setup h;
      setup h;
      Curl.set_writefunction h (fun s -> 
        try 
          match check h with 
          | false -> 0
          | true -> IO.nwrite out s; String.length s
        with exn -> inner := Some exn; 0);
      Curl.perform h
    end
  with
  | exn -> raise (Option.default exn !inner)

let http_get_io url ?(verbose=true) ?setup out =
  try
    http_get_io_exn url ?setup out
  with 
  | Curl.CurlException(Curl.CURLE_WRITE_ERROR,_,_) -> ()
  | exn -> if verbose then Log.main #warn ~exn "http_get_io(%s)" url else ()

let http_get ?verbose ?setup url = wrapped (IO.output_string ()) IO.close_out (http_get_io ?verbose ?setup url)

let http_gets ?(setup=ignore) ?(check=(fun _ -> true)) ?(result=(fun _ _ -> ())) url =
  with_curl begin fun h ->
    Curl.set_url h url;
    curl_default_setup h;
    let () = setup h in
    let b = Buffer.create 10 in
    Curl.set_writefunction h begin fun s ->
      match check h with
      | true -> Buffer.add_string b s; String.length s
      | false -> 0
    end;
    try
      Curl.perform h;
      let () = result h Curl.CURLE_OK in
      `Ok (Curl.get_httpcode h, Buffer.contents b)
    with
    | Curl.CurlException (code,_,_) ->
      let () = result h code in
      `Error code
  end
