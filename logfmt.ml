
let[@inline] needs_escape c =
  Char.code c < 0x20 || c = '"' || c = '\\'

let[@inline] needs_quotes c =
  c = ' ' || Char.code c >= 0x80

type cat = Safe | Has_space | Needs_escape

let categorize s : cat =
  let quote = ref false in
  try
    for i=0 to String.length s-1 do
      let c = String.unsafe_get s i in
      if needs_escape c then raise_notrace Exit;
      if needs_quotes c then quote := true
    done;
    if !quote then Has_space else Safe
  with Exit -> Needs_escape

let add_quoted_escaped buf v =
  Buffer.add_char buf '"';
  String.iter (fun c ->
    match c with
    | '"'  -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c when Char.code c < 0x20 -> Printf.bprintf buf "\\x%02x" (Char.code c)
    | c -> Buffer.add_char buf c
  ) v;
  Buffer.add_char buf '"'

let add_pair buf k v =
  Buffer.add_string buf k;
  Buffer.add_char buf '=';
  match categorize v with
  | Safe -> Buffer.add_string buf v
  | Has_space -> Printf.bprintf buf {|"%s"|} v
  | Needs_escape -> add_quoted_escaped buf v

let rec add_to_buffer buf (pairs:Logger.Pairs.t) : unit =
  match pairs with
  | [] -> ()
  | [k,v] -> add_pair buf k v
  | (k,v) :: pairs -> add_pair buf k v; Buffer.add_char buf ' '; (add_to_buffer [@tailcall]) buf pairs

let to_string pairs = match pairs with
  | [] -> ""
  | _ ->
    let buf = Buffer.create 32 in
    add_to_buffer buf pairs;
    Buffer.contents buf

module Parser = struct
  type t = {
    lb : Lexing.lexbuf;
    buf : Buffer.t;
  }

  let create () : t = { lb = Lexing.from_string ~with_positions:false ""; buf = Buffer.create 64 }

  let reset_to_string (self : t) s =
    Buffer.reset self.buf;
    let n = String.length s in
    if Bytes.length self.lb.Lexing.lex_buffer < n then (
      let len = min Sys.max_string_length (n * 2 + 10) in
      assert (len >= n);
      self.lb.lex_buffer <- Bytes.create len
    );
    Bytes.blit_string s 0 self.lb.lex_buffer 0 n;
    self.lb.lex_buffer_len <- n;
    self.lb.lex_curr_pos <- 0;
    self.lb.lex_abs_pos <- 0;
    self.lb.lex_eof_reached <- false

  let parse (self : t) (str : string) : _ list =
    reset_to_string self str;
    Logfmt_lex.token self.buf [] self.lb
end
