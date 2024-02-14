(** Stream of html elements *)

open Printf
open ExtLib

include HtmlStream_ragel

let show_attrs_quote c a =
  List.map (fun (k,v) -> sprintf " %s=%c%s%c" k c (Raw.project v) c) a |> String.concat ""

let show_raw_quote c elem =
  match elem with
  | Tag (name,attrs) -> sprintf "<%s%s>" name (show_attrs_quote c attrs)
  | Text t -> Raw.project t
  | Close name -> Printf.sprintf "</%s>" name
  | Script (attrs, s) -> sprintf "<script%s>%s</script>" (show_attrs_quote c attrs) s
  | Style (attrs, s) -> sprintf "<style%s>%s</style>" (show_attrs_quote c attrs) s

let show_raw' = show_raw_quote '\''
let show_raw = show_raw_quote '"'

let attrs_include attrs a =
  let a =
    if a |> List.exists (fun (_,v) -> String.contains v ' ') then
      a |> List.map (fun (k,v) -> Stre.nsplitc v ' ' |> List.map (fun s -> k,s)) |> List.flatten
    else
      a
  in
  match a with
  | [] -> true
  | _ ->
    let attrs = List.map (fun (k,v) -> (k, Stre.nsplitc (Raw.project v) ' ')) attrs in
    try
      List.for_all (fun (k,v) -> List.mem v (List.assoc k attrs)) a
    with
      Not_found -> false

let tag name ?(a=[]) = function
  | Tag (name',attrs) when name = name' -> attrs_include attrs a
  | _ -> false

let close name = function Close name' when name = name' -> true | _ -> false

let to_text ?(br=false) ?(strip=false) = function
  | Tag ("br",_) when br -> Some (Raw.inject "\n")
  | Tag _ -> None
  | Text x -> Some (if strip then Raw.inject (String.strip (Raw.project x)) else x)
  | Script _
  | Style _
  | Close _ -> None

(* let make_text l = wrapped_outs (fun out -> List.iter (Option.may (IO.nwrite out) $ Option.map Raw.project $ to_text) l) *)
let make_text ?br l =
  let fold e =
    let b = Buffer.create 10 in
    let (_:bool) = Enum.fold (fun s bos -> if not bos && s <> "\n" then Buffer.add_char b ' '; Buffer.add_string b s; s = "\n") true e in
    Buffer.contents b
  in
  List.enum l |> Enum.filter_map (to_text ?br ~strip:true) |>
  Enum.map Raw.project |> fold |> Raw.inject
