(** web utilities *)

open ExtLib
open Printf

open Prelude
open Control

let log = Log.self

let get_host = String.lowercase $ Neturl.url_host $ Neturl.parse_url
let rawurlencode = Netencoding.Url.encode ~plus:false
let urlencode = Netencoding.Url.encode ~plus:true
let urldecode = Netencoding.Url.decode

module T = struct

module Stream = ExtStream

(** scan stream until predicate is true
  @return matching element
  @raise Not_found on stream end *)
let rec stream_get f = parser
  | [< 'x when f x; t >] -> x
  | [< 'x; t >] -> stream_get f t
  | [< >] -> raise Not_found

(** see {!stream_get} *)
let rec stream_find f s = ignore (stream_get f s)

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

  (**  @return list of (link,title,description)

  raises exn on error *)
  type extract_full = string -> (string * string * string) Enum.t

  type t = { 
    request : ?num:int -> string -> string; 
    extract : string -> string Enum.t; 
    extract_full : extract_full;
  }

  open HtmlStream

  let google_full s =
    let s = parse & Stream.of_string s in
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
      with _ -> log #info "skipped" end;
      loop ()
    in
    begin try loop () with Not_found -> () | exn -> log #warn ~exn "google_full" end;
    List.rev !acc

  let google =
    let re = Pcre.regexp ~flags:[`CASELESS] "<h3 class=r><a href=\"([^\"]+)\" class=l" in
    { extract = Stre.enum_extract re;
      request = (fun ?(num=10) q ->
        sprintf "http://www.google.com/search?hl=en&q=%s&num=%u&btnG=Search&aq=f&oq=&aqi="
          (urlencode q) num);
      extract_full = List.enum $ google_full;
    }

  let google_day =
    let re = Pcre.regexp ~flags:[`CASELESS] "<h3 class=r><a href=\"([^\"]+)\" class=l" in
    { extract = Stre.enum_extract re;
      request = (fun ?(num=10) q ->
        sprintf "http://www.google.com/search?hl=en&q=%s&num=%u&btnG=Search&aq=f&oq=&aqi=&tbs=qdr:d,sbd:1"
          (urlencode q) num);
      extract_full = List.enum $ google_full;
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
    | `L l -> List.enum l
    | _ -> log #warn "unrecognized result"; Enum.empty ()

  let rss_source ~default fmt =
    let re = Pcre.regexp ~flags:[`CASELESS] "<item>.*?<link>([^<]+)</link>.*?</item>" in
    { extract = Stre.enum_extract re;
      request = (fun ?(num=default) q -> sprintf fmt (urlencode q) default);
      extract_full = parse_rss;
    }

  let bing = rss_source ~default:50 "http://www.bing.com/search?q=%s&count=%u&format=rss"
  let google_blogs = rss_source ~default:50 "http://blogsearch.google.com/blogsearch_feeds?q=%s&num=%u&hl=en&safe=off&output=rss"
  let boardreader = rss_source ~default:50 "http://boardreader.com/rss/%s?extended_search=1&s=time_desc&p=%u&format=RSS2.0"

  let by_name = function
  | "bing" -> bing
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

let http_get_io_exn ?(extra=ignore) ?(check=curl_ok) url out =
  let inner = ref None in
  try
    with_curl begin fun h ->
      let check = lazy (check h) in
      Curl.set_url h url;
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
    exn -> raise (Option.default exn !inner)

let http_get_io url ?extra out =
  try
    http_get_io_exn url ?extra out
  with 
  | Curl.CurlException(Curl.CURLE_WRITE_ERROR,_,_) -> ()
  | exn -> Log.main #warn ~exn "http_get_io(%s)" url

let http_get ?extra url = wrapped (IO.output_string ()) IO.close_out (http_get_io ?extra url)

