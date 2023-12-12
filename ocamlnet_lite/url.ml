(* adapted from https://gitlab.com/gerdstolpmann/lib-ocamlnet3/-/blob/4d1a8401bd40c17632128545e2aa4c880535e208/code/src/netstring/netencoding.ml#L993 *)
let hex_digits =
  [|
    '0';
    '1';
    '2';
    '3';
    '4';
    '5';
    '6';
    '7';
    '8';
    '9';
    'A';
    'B';
    'C';
    'D';
    'E';
    'F';
  |]

let to_hex2 k =
  (* Converts k to a 2-digit hex string *)
  let s = Bytes.create 2 in
  Bytes.set s 0 hex_digits.((k lsr 4) land 15);
  Bytes.set s 1 hex_digits.(k land 15);
  Bytes.unsafe_to_string s

let of_hex1 c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | _ -> raise Not_found

let url_encoding_re = Netstring_str.regexp "[^A-Za-z0-9_.!*-]"
let url_decoding_re = Netstring_str.regexp "\\+\\|%..\\|%.\\|%"

let encode ?(plus = true) s =
  Netstring_str.global_substitute url_encoding_re
    (fun r _ ->
      match Netstring_str.matched_string r s with
      | " " when plus -> "+"
      | x ->
          let k = Char.code x.[0] in
          "%" ^ to_hex2 k)
    s

let decode ?(plus = true) ?(pos = 0) ?len s =
  let s_l = String.length s in
  let s1 =
    if pos = 0 && len = None then s
    else
      let len = match len with Some n -> n | None -> s_l in
      String.sub s pos len
  in
  let l = String.length s1 in
  Netstring_str.global_substitute url_decoding_re
    (fun r _ ->
      match Netstring_str.matched_string r s1 with
      | "+" -> if plus then " " else "+"
      | _ -> (
          let i = Netstring_str.match_beginning r in
          (* Assertion: s1.[i] = '%' *)
          if i + 2 >= l then failwith "Web.Url.decode";
          let c1 = s1.[i + 1] in
          let c2 = s1.[i + 2] in
          try
            let k1 = of_hex1 c1 in
            let k2 = of_hex1 c2 in
            String.make 1 (Char.chr ((k1 lsl 4) lor k2))
          with Not_found -> failwith "Web.Url.decode"))
    s1

let url_split_re = Netstring_str.regexp "[&=]"

let dest_url_encoded_parameters parstr =
  let rec parse_after_amp tl =
    match tl with
    | Netstring_str.Text name
      :: Netstring_str.Delim "="
      :: Netstring_str.Text value
      :: tl' ->
        (decode name, decode value) :: parse_next tl'
    | Netstring_str.Text name
      :: Netstring_str.Delim "="
      :: Netstring_str.Delim "&"
      :: tl' ->
        (decode name, "") :: parse_after_amp tl'
    | [ Netstring_str.Text name; Netstring_str.Delim "=" ] ->
        [ (decode name, "") ]
    | _ -> failwith "Web.Url.dest_url_encoded_parameters"
  and parse_next tl =
    match tl with
    | [] -> []
    | Netstring_str.Delim "&" :: tl' -> parse_after_amp tl'
    | _ -> failwith "Web.Url.dest_url_encoded_parameters"
  in
  let toklist = Netstring_str.full_split url_split_re parstr in
  match toklist with [] -> [] | _ -> parse_after_amp toklist
