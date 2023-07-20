[@@@ocaml.warning "-38-32"]

module Raw = struct
  include Prelude.Fresh(String)()
  let length x = String.length @@ project x
  let is_empty x = "" = project x
end

type elem =
| Tag of (string * (string * Raw.t) list)
| Script of ((string * Raw.t) list * string) (* attributes and contents. TODO investigate script contents encoding *)
| Style of ((string * Raw.t) list * string)
| Text of Raw.t
| Close of string

type range = (int * int)

type ctx = { mutable lnum : int; }

let get_lnum ctx = ctx.lnum

let init () = { lnum = 1 }

%%{
 machine htmlstream;

 action mark { mark := !p }
 action mark_end { mark_end := !p }
 action tag_start { tag_start := !p }
 action tag { let s, _r = sub () in tag := String.lowercase_ascii s; attrs := []; }
 action close_tag { let s, _r = sub () in call @@ (Close (String.lowercase_ascii s), tag_range ()) }
 action directive { let s, _r = sub () in directive := (String.lowercase_ascii s); attrs := []; }
 action text { let s, r = sub () in call @@ (Text (Raw.inject s), r) }
 action key { let s, _r = sub () in key := String.lowercase_ascii s; }
 action store_attr { attrs := (!key, Raw.inject (if !mark < 0 then "" else let s, _r = sub() in s)) :: !attrs }
 action tag_done {
    match !tag with
    | "script" -> fhold; fgoto in_script;
    | "style" -> fhold; fgoto in_style;
    | "" -> ()
    | _ -> call @@ (Tag (!tag, List.rev !attrs), tag_range ())
 }
 action tag_done_2 { let r = tag_range () in call @@ (Tag (!tag, List.rev !attrs), r); if !tag <> "a" then call ((Close !tag), r) }
 action directive_done { (* printfn "directive %s" !directive; *) }

 action garbage_tag { (*printfn "GARBAGE %S" (current ()); *) fhold; fgoto garbage_tag;}

 count_newlines = ('\n' >{ ctx.lnum <- ctx.lnum + 1 } | ^'\n'+)**;

 wsp = 0..32;
 ident = alnum | '-' | [_:] ;

 in_script := (count_newlines | any* >mark %mark_end :>> ('<' wsp* '/' wsp* 'script'i wsp* '>' >{
  let s, _r = sub () in call @@ (Script (List.rev !attrs, s), tag_range ())
 } @{fgoto main;}));
 in_style := (count_newlines | any* >mark %mark_end :>> ('<' wsp* '/' wsp* 'style'i wsp* '>' >{
  let s, _r = sub () in call @@ (Style (List.rev !attrs, s), tag_range ())
 } @{fgoto main;}));

 garbage_tag := (count_newlines | ^'>'* '>' @tag_done @{ fgoto main; });

 literal = ( "'" ^"'"* >mark %mark_end "'" | '"' ^'"'* >mark %mark_end '"' | ^(wsp|'"'|"'"|'>')+ >mark %mark_end);
 tag_attrs = (wsp+ | ident+ >mark %key wsp* ('=' wsp* literal)? %store_attr )**;
 close_tag = '/' wsp* ident* >mark %mark_end <: ^'>'* '>' %close_tag;
 open_tag = ident+ >mark %tag <: wsp* tag_attrs ('/' wsp* '>' %tag_done_2 | '>' %tag_done);
 directive = ('!'|'?') (alnum ident+) >mark %directive <: wsp* tag_attrs '?'? '>' %directive_done;
 comment = "!--" any* :>> "-->";
 # reset tag so that garbage_tag will not generate duplicate tag with tag_done
 tag = '<' >tag_start wsp* <: (close_tag | open_tag | directive | comment) @lerr(garbage_tag) >{ tag := "" };
 main := (((tag | ^'<' >mark ^'<'* %text ) )** | count_newlines);

 write data;
}%%

(** scan [data] for html tags and invoke [call] for every element  *)
let parse_with_range ?(ctx=init ()) call data =
  let cs = ref 0 in
  let mark = ref (-1) in
  let mark_end = ref (-1) in
  let tag_start = ref (-1) in 
  let tag = ref "" and key = ref "" and attrs = ref [] and directive = ref "" in
(*  let substr data ofs len = try String.sub data ofs len with exn -> Prelude.printfn "%S %d %d %d" data (String.length data) ofs len; raise exn in *)
  let substr = String.sub in
  %%write init;
  let eof = ref (String.length data) in
  let p = ref 0 in
  let pe = ref (String.length data) in
  let sub () =
    assert (!mark >= 0);
    if !mark_end < 0 then mark_end := !p;
    let s = if !mark_end <= !mark then "" else substr data !mark (!mark_end - !mark) in
    let range = (!mark, !mark_end) in
    mark := -1;
    mark_end := -1;
    (s, range)
  in
  let tag_range () = 
    assert (!tag_start >= 0);
    let range = (!tag_start, !p) in
    tag_start := -1;
    range
  in
  %%write exec;
(* FIXME ? *)
(*     if !eof <> -1 && !cs < htmlstream_first_final then Exn.fail "not parsed"; *)
  ()

(* vim: ft=ocaml
*)
