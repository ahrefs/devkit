
open ExtLib
open Printf

open Prelude
open Control

let log = Log.self

module HtmlStream = struct

type elem = Tag of (string * (string*string) list) | Text of string | Close of string

let eq : char -> char -> bool = (=)
let neq : char -> char -> bool = (<>)
let is_alpha = function 
  | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true 
  | _ -> false
let is_ws = function
  | c when Char.code c <= 32 -> true
  | _ -> false

exception EndTag

let label = String.lowercase

let rec parse = parser
  | [< ''<'; x = tag; t >] -> [< 'x; parse t >]
  | [< 'c; x = chars c (neq '<'); t >] -> [< 'Text x; parse t >]
  | [< >] -> [< >]
  and tag = parser
  | [< 'c when is_alpha c; name = chars c is_alpha; () = skip is_ws; a=tag_attrs [] >] -> Tag (label name,List.rev a)
  | [< ''/'; () = skip is_ws; x = close_tag >] -> Close (label x)
  | [< t >] -> skip_till '>' t; Tag ("",[]) (* skip garbage *)
  and close_tag = parser
  | [< 'c when is_alpha c; name = chars c is_alpha; () = skip_till '>' >] -> name
  | [< t >] -> skip_till '>' t; ""
  and tag_attrs a = parser
  | [< ''>' >] -> a
  | [< 'c when is_alpha c; name = chars c is_alpha; () = skip is_ws; t >] ->
    begin match try Some (maybe_value t) with EndTag -> None with
    | Some v -> skip is_ws t; tag_attrs ((label name,v) :: a) t
    | None -> (label name,"") :: a
    end
  | [< t >] -> skip_till '>' t; a (* skip garbage *)
  and maybe_value = parser
  | [< ''>' >] -> raise EndTag
  | [< ''='; () = skip is_ws; t >] -> parse_value t
  | [< >] -> ""
  and parse_value = parser
  | [< ''\''; s = till '\'' >] -> s
  | [< ''"'; s = till '"' >] -> s
  | [< ''>' >] -> raise EndTag
  | [< 'c when is_alpha c; s = chars c is_alpha >] -> s
  | [< t >] -> skip_till '>' t; raise EndTag (* skip garbage *)
  (** @return all that match [f] *)
  and chars c f strm =
    let b = Buffer.create 10 in
    Buffer.add_char b c;
    let rec loop () =
      match Stream.peek strm with
      | Some c when f c -> Stream.junk strm; Buffer.add_char b c; loop ()
      | None -> Buffer.contents b
      | _ -> Buffer.contents b 
    in loop ()
  (** @return everything till [delim] (consumed but not included) *)
  and till delim strm =
    let b = Buffer.create 10 in
    let rec loop () =
      let c = Stream.next strm in
      if c = delim then () else (Buffer.add_char b c; loop ())
    in 
    begin try loop () with Stream.Failure -> () end;
    Buffer.contents b
  (** skip all that match [f] *)
  and skip f = parser
  | [< 'c when f c; t >] -> skip f t
  | [< >] -> ()
  (** skip all till [delim] (including) *)
  and skip_till delim strm =
    if try Stream.next strm = delim with Stream.Failure -> true then () else skip_till delim strm

(** convert char stream to html elements stream.
  Names (tags and attributes) are lowercased *)
let parse s = try parse s with exn -> log #warn ~exn "HtmlStream.parse"; [< >]

(* open Printf *)

let quote =
  let rex = Pcre.regexp "['\"&]" in
  Pcre.substitute ~rex ~subst:(function "'" -> "&apos;" | "\"" -> "&quot;" | "&" -> "&amp;" | _ -> assert false)

let show c elem = 
  match elem with
  | Tag (name,attrs) ->
    wrapped_output (IO.output_string ()) begin fun out ->
      IO.printf out "<%s" name; 
      List.iter (fun (k,v) -> IO.printf out " %s=%c%s%c" k c (quote v) c) attrs;
      IO.printf out ">"
    end
  | Text t -> t
  | Close name -> sprintf "</%s>" name

let show' = show '\''
let show = show '"'

let rec show_stream = parser
  | [< 'c; t >] -> Printf.printf "-> %c\n%!" c; [< 'c; show_stream t >]
  | [< >] -> [< >] 

let dump = Stream.iter (print_endline $ show) $ parse $ 
(*   show_stream $ *)
  Stream.of_string

let tag name ?(a=[]) = function
  | Tag (name',attrs) when name = name' -> 
    begin try List.for_all (fun (k,v) -> List.assoc k attrs = v) a with Not_found -> false end
  | _ -> false

let close name = function Close name' when name = name' -> true | _ -> false 

let rec stream_get f = parser
  | [< 'x when f x; t >] -> x
  | [< 'x; t >] -> stream_get f t
  | [< >] -> raise Not_found

let rec stream_find f s = ignore (stream_get f s)

let rec stream_skip f = parser
  | [< 'x when f x; t >] -> stream_skip f t
  | [< >] -> ()

let stream_get_next f = parser
  | [< 'x when f x >] -> x
  | [< >] -> raise Not_found

let stream_next f s = ignore (stream_get_next f s)

let stream_extract_while f s =
  let rec loop acc = 
    match Stream.peek s with
    | Some x when f x -> Stream.junk s; loop (x::acc)
    | _ -> List.rev acc
  in
  loop []

let stream_extract_till x = stream_extract_while ((<>) x)

let to_text = function
  | Tag _ -> None
  | Text x -> Some x
  | Close _ -> None

let make_text l = wrapped_outs (fun out -> List.iter (Option.may (IO.nwrite out) $ to_text) l)

end

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
          (Netencoding.Url.encode q) num);
      extract_full = List.enum $ google_full;
    }

  let google_day =
    let re = Pcre.regexp ~flags:[`CASELESS] "<h3 class=r><a href=\"([^\"]+)\" class=l" in
    { extract = Stre.enum_extract re;
      request = (fun ?(num=10) q ->
        sprintf "http://www.google.com/search?hl=en&q=%s&num=%u&btnG=Search&aq=f&oq=&aqi=&tbs=qdr:d,sbd:1"
          (Netencoding.Url.encode q) num);
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
      request = (fun ?(num=default) q -> sprintf fmt (Netencoding.Url.encode q) default);
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

let get_host = String.lowercase $ Neturl.url_host $ Neturl.parse_url
let urlencode = Netencoding.Url.encode
let urldecode = Netencoding.Url.decode

let () = Curl.global_init Curl.CURLINIT_GLOBALALL

let with_curl f =
  bracket (Curl.init ()) Curl.cleanup f

let http_get_io url out =
  try
    with_curl begin fun h ->
      Curl.set_url h url;
      Curl.set_writefunction h (fun s -> IO.nwrite out s; String.length s);
      Curl.perform h
    end
  with
    exn -> Log.main #warn ~exn "http_get_io(%s)" url

let http_get url = wrapped (IO.output_string ()) IO.close_out (http_get_io url)

