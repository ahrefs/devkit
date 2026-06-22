{
}

rule token buf acc = parse
  | ' '+
    { token buf acc lexbuf }
  | eof
    { List.rev acc }
  | ([^ '=' ' ']+ as key) '=' '"'
    { Buffer.clear buf;
      let value = quoted buf lexbuf in
      token buf (( key, value ) :: acc) lexbuf }
  | ([^ '=' ' ']+ as key) '=' ([^ '"' ' '][^ ' ']* as value)
    { token buf (( key, value ) :: acc) lexbuf }
  | ([^ '=' ' ']+ as key) '='
    { token buf (( key, "" ) :: acc) lexbuf }
  | ([^ '=' ' ']+ as key)
    { token buf (( key, "" ) :: acc) lexbuf }
  | _
    { token buf acc lexbuf }

and quoted buf = parse
  | '"'
    { Buffer.contents buf }
  | eof
    { Buffer.contents buf }
  | '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F'] as h1) (['0'-'9' 'a'-'f' 'A'-'F'] as h2)
    { let hex c = if c <= '9' then Char.code c - 48 else (Char.code c land 0x0f) + 9 in
      Buffer.add_char buf (Char.chr (hex h1 * 16 + hex h2));
      quoted buf lexbuf }
  | '\\' (_ as c)
    { let c' = match c with 'n' -> '\n' | 'r' -> '\r' | 't' -> '\t' | c -> c in
      Buffer.add_char buf c'; quoted buf lexbuf }
  | _ as c
    { Buffer.add_char buf c; quoted buf lexbuf }

{
}

