
open ExtLib
open Printf

open Prelude
open Control

(* let log = Log.from "web" *)

module HtmlStream = struct

type elem = Tag of (string * (string*string) list) | Text of string

let eq : char -> char -> bool = (=)
let neq : char -> char -> bool = (<>)
let is_alpha = function 
  | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true 
  | _ -> false
let is_ws = function
  | c when Char.code c <= 32 -> true
  | _ -> false

exception EndTag

let rec make = parser
  | [< ''<'; x = tag; t >] -> [< 'Tag x; make t >]
  | [< x = till '<'; xtag = tag; t >] -> [< 'Text x; 'Tag xtag; make t >]
  | [< >] -> [< >]
  and tag = parser
  | [< 'c when is_alpha c; name = chars c is_alpha; () = skip is_ws; a=tag_attrs [] >] -> (name,a)
  | [< t >] -> skip_till '>' t; ("",[]) (* skip garbage *)
  and tag_attrs a = parser
  | [< ''>' >] -> a
  | [< 'c when is_alpha c; name = chars c is_alpha; () = skip is_ws; t >] ->
    begin match try Some (maybe_value t) with EndTag -> None with
    | Some v -> skip is_ws t; tag_attrs ((name,v) :: a) t
    | None -> (name,"") :: a
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
      | None -> raise Stream.Failure 
      | _ -> Buffer.contents b 
    in loop ()
  (** @return everything till [delim] (consumed but not included) *)
  and till delim strm =
    let b = Buffer.create 10 in
    let rec loop () =
      let c = Stream.next strm in
      if c = delim then Buffer.contents b else (Buffer.add_char b c; loop ())
    in loop ()
  (** skip all that match [f] *)
  and skip f = parser
  | [< 'c when f c; t >] -> skip f t
  | [< >] -> ()
  (** skip all till [delim] (including) *)
  and skip_till delim strm =
    if Stream.next strm = delim then () else skip_till delim strm

(* let make s = try make s with Stream.Failure ->  *)

(* open Printf *)

let rex = Pcre.regexp "\""

let show elem = 
  match elem with
  | Tag (name,attrs) ->
    wrapped_output (IO.output_string ()) begin fun out ->
      IO.printf out "<%s" name; 
      List.iter (fun (k,v) -> IO.printf out " %s=\"%s\"" k (Pcre.replace ~rex ~templ:"&quot;" v)) attrs;
      IO.printf out ">"
    end
  | Text t -> t

let rex = Pcre.regexp "'"

let show' elem =
  match elem with
  | Tag (name,attrs) ->
    wrapped_output (IO.output_string ()) begin fun out ->
      IO.printf out "<%s" name; 
      List.iter (fun (k,v) -> IO.printf out " %s='%s'" k (Pcre.replace ~rex ~templ:"&apos;" v)) attrs;
      IO.printf out ">"
    end
  | Text t -> t

let rec show_stream = parser
  | [< 'c; t >] -> Printf.printf "-> %c\n%!" c; [< 'c; show_stream t >]
  | [< >] -> [< >] 

let test = Stream.iter (print_endline $ show) $ make $ 
(*   show_stream $ *)
  Stream.of_string

end

module Provider = struct

  (**  @return list of (link,title,description)

  raises exn on error *)
  type extract_full = string -> (string * string * string) Enum.t

  type t = { 
    request : string -> string; 
    extract : string -> string Enum.t; 
    extract_full : extract_full;
  }

(*
  let google_full =
    tag "h3" ~with:["class","r"] 
    let re = Pcre.regexp ~flags:[`CASELESS] "<h3 class=r><a href=\"([^\"]+)\" class=l" in
    fun s ->
*)

    

  let google =
    let re = Pcre.regexp ~flags:[`CASELESS] "<h3 class=r><a href=\"([^\"]+)\" class=l" in
    { extract = Stre.enum_extract re;
      request = (fun q ->
        sprintf "http://www.google.com/search?hl=en&q=%s&btnG=Search&aq=f&oq=&aqi="
          (Netencoding.Url.encode q));
      extract_full = fun q -> assert false;
    }

  let bing_full s =
    let xml = Xmlm.make_input (`String (0,s)) in
    let rec skip () = 
      match Xmlm.peek xml with
      | `Dtd _ -> ignore & Xmlm.input xml; skip ()
      | _ -> () in
    skip ();
    let link = ref "" and title = ref "" and desc = ref "" in
    match Xmlm.input_tree ~data:(fun s -> `D s) ~el:(fun ((_,name),_) ch ->
      match name,ch with
      | "item", _ -> let r = (!link, !title, !desc) in link := ""; title := ""; desc := ""; `R r
      | "link",[`D s] -> link := s; `U
      | "description", [`D s] -> desc := s; `U
      | "title", [`D s] -> title := s; `U
      | "channel", l -> `L (List.filter_map (function (`R x) -> Some x | _ -> None) l)
      | "rss",[x] -> x
      | _ -> `U) xml
    with
    | `L l -> List.enum l
    | _ -> assert false

  let bing =
    let re = Pcre.regexp ~flags:[`CASELESS] "<item>.*?<link>([^<]+)</link>.*?</item>" in
    { extract = Stre.enum_extract re;
      request = (fun q ->
        sprintf "http://www.bing.com/search?q=%s&count=50&format=rss" (Netencoding.Url.encode q));
      extract_full = bing_full;
    }

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

