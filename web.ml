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

module T = struct

module Stream = ExtStream

(** scan stream until predicate is true
  @param limit skip not more than [limit] elements
  @return matching element
  @raise Not_found on stream end *)
let rec stream_get ?(limit=max_int) f = parser
  | [< 'x when f x; t >] -> x
  | [< 'x; t >] -> if limit = 0 then raise Not_found else stream_get ~limit:(limit-1) f t
  | [< >] -> raise Not_found

(** see {!stream_get} *)
let rec stream_find ?limit f s = ignore (stream_get ?limit f s)

(** scan stream while predicate holds, stop on first non-matching element or stream end *)
let rec stream_skip f = parser
  | [< 'x when f x; t >] -> stream_skip f t
  | [< >] -> ()

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

end (* T *)

include T

module Provider = struct

  (**  @return total estimate and list of (link,title,description)

  raises exn on error *)
  type extract_full = string -> int * (string * string * string) Enum.t

  type t = { 
    request : ?num:int -> string -> string; 
    extract : string -> string Enum.t; 
    extract_full : extract_full;
  }

  open HtmlStream

  let google_full s =
    let s = parse & Stream.of_string s in
    let total = ref 0 in
    begin try
      stream_find (tag "div" ~a:["id","resultStats"]) s;
      match Stream.next s with
      | Text s ->
        total := String.explode s >> List.filter (function '0'..'9' -> true | _ -> false) >> String.implode >> int_of_string
      | _ -> ()
    with exn -> log #warn ~exn "bad resultStats" end;
    let acc = ref [] in
    let rec loop () =
      stream_find (tag "li" ~a:["class","g"]) s;
      begin try
        stream_next (tag "h3" ~a:["class","r"]) s;
        let href = match stream_get_next (tag "a" ~a:["class","l"]) s with
        | Tag (_,l) -> List.assoc "href" l
        | _ -> assert false in
        let h = stream_extract_till (Close "h3") s in
        stream_find (tag "div"(* ~a:["class","s"]*)) s;
        if tag "span" ~a:["class","f"] (Option.get & Stream.peek s) then stream_find (tag "br") s;
        let t = stream_extract_while (not $ tag "br") s in
        acc := (href,make_text h,make_text t) :: !acc;
      with exn -> log #debug ~exn "skipped search result" end;
      loop ()
    in
    begin try loop () with Not_found -> () | exn -> log #warn ~exn "google_full" end;
    !total, List.enum & List.rev !acc

  let google =
    let re = Pcre.regexp ~flags:[`CASELESS] "<h3 class=r><a href=\"([^\"]+)\" class=l" in
    { extract = Stre.enum_extract re;
      request = (fun ?(num=10) q ->
        sprintf "http://www.google.com/search?hl=en&q=%s&num=%u&btnG=Search&aq=f&oq=&aqi="
          (urlencode q) num);
      extract_full = google_full;
    }

  let google_day =
    let re = Pcre.regexp ~flags:[`CASELESS] "<h3 class=r><a href=\"([^\"]+)\" class=l" in
    { extract = Stre.enum_extract re;
      request = (fun ?(num=10) q ->
        sprintf "http://www.google.com/search?hl=en&q=%s&num=%u&btnG=Search&aq=f&oq=&aqi=&tbs=qdr:d,sbd:1"
          (urlencode q) num);
      extract_full = google_full;
    }

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
      | "item", _ -> let r = (!link, !title, !desc) in link := ""; title := ""; desc := ""; `R r
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
    | `L l -> 0, List.enum l
    | _ -> log #warn "unrecognized result"; 0, Enum.empty ()

  let bing_html s =
    let s = parse & Stream.of_string s in
    let total = ref 0 in
    begin try
      stream_find (tag "span" ~a:["class","sb_count"]) s;
      match Stream.next s with
      | Text s ->
        total := Scanf.sscanf s "%_s@ of %s@ " (fun s ->
          int_of_string & String.replace_chars (function '0'..'9' as c -> String.make 1 c | _ -> "_") s)
      | _ -> ()
    with exn -> log #debug ~exn "bad sb_count" end;
    let acc = ref [] in
    let rec loop () =
      stream_find (tag "div" ~a:["class","sb_tlst"]) s;
      begin try
        stream_next (tag "h3") s;
        let href = match stream_get_next (tag "a") s with
        | Tag (_,l) -> List.assoc "href" l
        | _ -> assert false in
        let h = stream_extract_till (Close "h3") s in
        let t = stream_extract_till (Close "p") s in
        acc := (href,make_text h,make_text t) :: !acc;
      with exn -> log #debug ~exn "skipped search result" end;
      loop ()
    in
    begin try loop () with Not_found -> () | exn -> log #warn ~exn "bing_html" end;
    !total, List.enum & List.rev !acc

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

  let by_name ?lang = function
  | "bing" (* -> bing *)
  | "bing_html" -> bing_html lang
  | "google" -> google
  | "google_blogs" -> google_blogs
  | "google_day" -> google_day
  | "boardreader" -> boardreader
  | s -> Exn.fail "unknown search provider : %s" s

end

module Search(GET : sig val get : string -> string end) = struct

  open Provider

  let search p = p.extract $ GET.get $ p.request

  let google = search google
  let bing = search bing

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

let http_get_io_exn ?(extra=ignore) ?(check=curl_ok) url out =
  let inner = ref None in
  try
    with_curl begin fun h ->
      let check = lazy (check h) in
      Curl.set_url h url;
      curl_default_setup h;
      extra h;
      Curl.set_writefunction h (fun s -> 
        try 
          match Lazy.force check with 
          | false -> 0
          | true -> IO.nwrite out s; String.length s
        with exn -> inner := Some exn; 0);
      Curl.perform h
    end
  with
  | exn -> raise (Option.default exn !inner)

let http_get_io url ?(verbose=true) ?extra out =
  try
    http_get_io_exn url ?extra out
  with 
  | Curl.CurlException(Curl.CURLE_WRITE_ERROR,_,_) -> ()
  | exn -> if verbose then Log.main #warn ~exn "http_get_io(%s)" url else ()

let http_get ?verbose ?extra url = wrapped (IO.output_string ()) IO.close_out (http_get_io ?verbose ?extra url)

let http_gets ?(setup=ignore) url =
  try
  with_curl begin fun h ->
    Curl.set_url h url;
    curl_default_setup h;
    setup h;
    let b = Buffer.create 10 in
    Curl.set_writefunction h (fun s -> Buffer.add_string b s; String.length s);
    Curl.perform h;
    `Ok (Curl.get_httpcode h, Buffer.contents b)
  end
  with
  | Curl.CurlException (code,_,_) -> `Error code


