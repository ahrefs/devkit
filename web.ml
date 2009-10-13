
open ExtLib
open Printf

open Prelude

module Search = struct

  module MakeSimple(T : sig val request : string -> string val extract : string -> string Enum.t end) = struct
    let search get = T.extract & get & T.request
  end

  let all_matches rex s =
    try
      Pcre.exec_all ~rex s >> Array.enum >> Enum.map (flip Pcre.get_substring 1)
    with
      _ -> Enum.empty ()

  module Google = MakeSimple(struct
    let re = Pcre.regexp ~flags:[`CASELESS] "<h3 class=r><a href=\"([^\"]+)\" class=l"
    let extract = all_matches re
    let request q =
      sprintf "http://www.google.com/search?hl=en&q=%s&btnG=Search&aq=f&oq=&aqi="
        (Netencoding.Url.encode q)
  end)

  module Bing = MakeSimple(struct
    let re = Pcre.regexp ~flags:[`CASELESS] "<link>([^<]+)</link>"
    let extract = all_matches re 
    let request q =
      sprintf "http://www.bing.com/search?q=%s&count=50&format=rss" (Netencoding.Url.encode q)
  end)

end

