
open ExtLib
open Printf

open Prelude

module Provider = struct

  type t = { request : string -> string; extract : string -> string Enum.t; }

  let all_matches rex s =
    try
      Pcre.exec_all ~rex s >> Array.enum >> Enum.map (flip Pcre.get_substring 1)
    with
      _ -> Enum.empty ()

  let google =
    let re = Pcre.regexp ~flags:[`CASELESS] "<h3 class=r><a href=\"([^\"]+)\" class=l" in
    { extract = all_matches re;
      request = fun q ->
        sprintf "http://www.google.com/search?hl=en&q=%s&btnG=Search&aq=f&oq=&aqi="
          (Netencoding.Url.encode q)
    }

  let bing =
    let re = Pcre.regexp ~flags:[`CASELESS] "<item>.*?<link>([^<]+)</link>.*?</item>" in
    { extract = all_matches re;
      request = fun q ->
        sprintf "http://www.bing.com/search?q=%s&count=50&format=rss" (Netencoding.Url.encode q)
    }

end

module Search(GET : sig val get : string -> string end) = struct

  open Provider

  let search p = p.extract & GET.get & p.request

  let google = search google
  let bing = search bing

end

