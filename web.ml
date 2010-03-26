
open ExtLib
open Printf

open Prelude
open Control

(* let log = Log.from "web" *)

module Provider = struct

  type t = { request : string -> string; extract : string -> string Enum.t; }

  let google =
    let re = Pcre.regexp ~flags:[`CASELESS] "<h3 class=r><a href=\"([^\"]+)\" class=l" in
    { extract = Stre.enum_extract re;
      request = fun q ->
        sprintf "http://www.google.com/search?hl=en&q=%s&btnG=Search&aq=f&oq=&aqi="
          (Netencoding.Url.encode q)
    }

  let bing =
    let re = Pcre.regexp ~flags:[`CASELESS] "<item>.*?<link>([^<]+)</link>.*?</item>" in
    { extract = Stre.enum_extract re;
      request = fun q ->
        sprintf "http://www.bing.com/search?q=%s&count=50&format=rss" (Netencoding.Url.encode q)
    }

  (** 
    @return list of (link,title,description)

    raises exn on error *)
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

