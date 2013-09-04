
open Xhtml.M
open ExtLib
open Printf

open Prelude

let t = pcdata

let user_input name data = input ~a:[a_input_type `Text; a_name name; a_value data] ()
let ahref s = a ~a:[a_href (uri_of_string s)]

let document heading extra x =
  html ~a:[a_xmlns `W3_org_1999_xhtml; a_xml_lang "en"]
  (head (title (t heading)) extra)
  (body x)

let doc heading x = document heading [] x

let spn cls = span ~a:[a_class [cls]]

(*
let output = pretty_print
let output_doc out heading x = output out (doc heading x)
let output_html out h = XML.pretty_print ~preformatted ~no_break out (toelt h)
*)

let to_html () = 
  [
(*
  p
    [t (sprintf "%u varz" (Varz.values () >> Enum.count))];
*)
  p
    (Varz.values () >> List.map (fun (k,v) -> spn "nvp" [spn "name" [t k]; spn "value" [t v]; br ()]));
(*
  p
    [t (sprintf "%u varz" (Varz.controls () >> Enum.count))];
*)
  div
      [form ~action:(uri_of_string "") ~a:[a_method `Post]
      (p
      (Varz.controls () >> List.map (fun (k,v) -> 
        spn "nvp" [spn "name" [t k]; spn "value" [user_input k v]; br ()])))
      [
        div [
          input ~a:[a_name "submit"; a_value "1"; a_input_type `Hidden] ();
          input ~a:[a_value "Submit"; a_input_type `Submit] ();]
      ]]
  ]

let to_html_doc () =
  document "Varz" [
    style "text/css" (List.map t [
      ".nvp .name {font-weight:bold;padding-right:2em;}";
      ".nvp .value {}";
      ".nvp .value input {width:10em;}";
    ]
  )]
  (to_html ())

(*
  let module Arg = Netcgi_ext.Cgi_arg(struct let cgi = cgi end) in

  let () =
  match Arg.get "submit" with
  | Some "1" -> 
    Varz.controls () >> List.iter (fun (k,v) ->
      Option.may (fun set -> if set <> v then ignore (Varz.set_control k set)) (Arg.get k))
  | _ -> ()
  in
*)
