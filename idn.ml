(* Punycode and IDN library for OCaml *)
(* License: without restrictions      *)
(* Author: dima@caml.ru               *)

(* Fixes by: ygrek and cyberhuman *)
(* Version: 2013/08/29 *)

module type CONV = sig
val upoints : string -> int array
val ustring : int array -> string
end

module Make(CONV : CONV) = struct

exception Bad_input
exception Overflow

(* Parameters *)

let base = 36
let tmin = 1
let tmax = 26
let skew = 38
let damp = 700
let initial_bias = 72
let initial_n = 0x80
let delimiter = 0x2D

(* Encoding *)

let basic p = p < 0x80

let encode_digit d =
  if d < 26 then
    d + Char.code 'a'
  else if d < 36 then
    d - 26 + Char.code '0'
  else
    raise Bad_input

let adapt delta num_points first =
  let delta = if first then delta / damp else (delta lsr 1) in
  let delta = ref (delta + (delta / num_points)) in
  let k = ref 0 in
  let lim = ((base - tmin) * tmax) / 2 in
  while (!delta > lim) do
    delta := !delta / (base - tmin);
    k := !k + base
  done;
  !k + (((base - tmin + 1) * !delta) / (!delta + skew))

let encode_data input_data =
  let n = ref initial_n in
  let delta = ref 0 in
  let bias = ref initial_bias in
  let basic_count = ref 0 in
  let buf = Buffer.create 32 in
  let out n =
    Buffer.add_char buf (Char.chr n) in

  Array.iter
    (fun c ->
      if basic c then
	begin
	  out c;
	  incr basic_count;
	end)
    input_data;

  if !basic_count > 0 then
    Buffer.add_char buf (Char.chr delimiter);

  let handled_count = ref !basic_count in

  while (!handled_count < Array.length input_data) do
    let m = ref max_int in
    Array.iter
      (fun c ->
	if c >= !n && c < !m then
	  m := c)
      input_data;

    if !m - !n > (max_int - !delta) / (succ !handled_count) then
      raise Overflow;
    delta := !delta + (!m - !n) * (succ !handled_count);
    n := !m;

    Array.iter
      (fun c ->
	if c < !n then
	  begin
	    incr delta;
	    if !delta = 0 then
	      raise Overflow;
	  end;
	if c = !n then
	  begin
	    let q = ref !delta in
	    let k = ref base in
	    (try
	      while true do
		let t =
		  if !k <= !bias then tmin
		  else if !k >= !bias + tmax then tmax
		  else !k - !bias in
		if !q < t then
		  raise Exit;
		out (encode_digit (t + ((!q - t) mod (base - t))));
		q := (!q - t) / (base - t);
		k := !k + base
	      done
	    with Exit -> ());
	    out (encode_digit !q);
	    bias := adapt !delta (succ !handled_count) (!handled_count = !basic_count);
	    delta := 0;
	    incr handled_count;
	  end)
      input_data;
    incr delta;
    incr n;
  done;
  Buffer.contents buf

(* Decoding *)

let decode_digit p =
  if p < 48 then raise Bad_input else
  if p < 58 then p + 26 - 48 else
  if p < 65 then raise Bad_input else
  if p < 65 + 26 then p - 65 else
  if p < 97 then raise Bad_input else
  if p < 97 + 26 then p - 97 else
    raise Bad_input

let decode_data input_data =
  let buflen = String.length input_data in
  let n = ref initial_n in
  let i = ref 0 in
  let bias = ref initial_bias in
  let buf = Array.make buflen 0 in

  let input_length =
    String.length input_data in

  let out = ref 0 in
  let data_pos =
    try
      let pos = String.rindex input_data (Char.chr delimiter) in
      for i = 0 to pos - 1 do
        Array.unsafe_set buf i (Char.code input_data.[i])
      done;
      out := pos;
      pos + 1
    with _ -> 0
  in

  let j = ref data_pos in
  while !j < input_length do
    let oldi = ref !i in
    let w = ref 1 in
    let k = ref base in
    (try
      while true do
	if !j >= input_length then raise Bad_input;
	let digit = decode_digit (Char.code input_data.[!j]) in incr j;
	if digit > (max_int - !i) / !w then raise Overflow;
	i := !i + digit * !w;
	let t =
	  if !k <= !bias then tmin
	  else if !k >= !bias + tmax then tmax
	  else !k - !bias
	in
	if digit < t then
	  raise Exit;
	if !w > max_int / (base - t) then raise Overflow;
	w := !w * (base - t);
	k := !k + base
      done
    with Exit -> ());
    let next = succ !out in
    bias := adapt (!i - !oldi) next (!oldi = 0);
    if !i / next > max_int - !n then raise Overflow;
    n := !n + !i / next;
    i := !i mod next;
    if !out >= buflen then raise Overflow;
    if !out > !i then
      Array.blit buf !i buf (!i + 1) (!out - !i);
    buf.(!i) <- !n;
    incr i; incr out;
  done;
  Array.sub buf 0 !out

(* Helpers *)

let split domain =
  let rec make acc rest =
    try
      let pos = String.index rest '.' in
      make ((String.sub rest 0 pos)::acc)
	(String.sub rest (succ pos) ((String.length rest) - pos - 1))
    with Not_found -> List.rev (rest::acc)
  in make [] domain

let join = String.concat "."

let need_encoding s =
  let l =
    String.length s in
  try
    for i = 0 to pred l do
      if not (basic (Char.code (String.unsafe_get s i))) then
        raise Exit
    done; false
  with Exit -> true

let need_decoding s =
  let l = String.length s in
  try
    if l >= 4 then
      if (String.unsafe_get s 0 = 'x')
      && (String.unsafe_get s 1 = 'n')
      && (String.unsafe_get s 2 = '-')
      && (String.unsafe_get s 3 = '-')
      then raise Exit
      else
        for i = 0 to pred l - 4 do
          if (String.unsafe_get s i = '.')
          && (String.unsafe_get s (i+1) = 'x')
          && (String.unsafe_get s (i+2) = 'n')
          && (String.unsafe_get s (i+3) = '-')
          && (String.unsafe_get s (i+4) = '-')
          then raise Exit
        done;
    false
  with Exit -> true

(* Punycode API *)

let encode s = encode_data (CONV.upoints s)
let decode s = CONV.ustring (decode_data s)

let transcode s =
  if need_encoding s then
    "xn--" ^ encode s
  else s

let transtext s =
  let l = String.length s in
  if l > 4 && String.sub s 0 4 = "xn--" then
    decode (String.sub s 4 (l - 4))
  else s

(* IDN api *)

let encode_domain domain =
  if need_encoding domain then
    join (List.map transcode (split domain))
  else
    domain

let decode_domain domain =
  if need_decoding domain then
    join (List.map transtext (split domain))
  else domain

let self_test () =
  assert ("他们为什么不说中文" = decode "ihqwcrb4cv8a8dqg056pqjye");
  assert ("---禁刊拍賣網址---" = decode "-------5j3ji85am9zsk4ckwjm29b");
  assert ("reality44hire-b9a" = encode (decode "reality44hire-b9a"));
  assert (need_encoding "---禁刊拍賣網址---");
  assert (not (need_encoding "asdasdasdfs"));
  assert (need_decoding "xn--asd.asda");
  assert (need_decoding "qwe.xn--werw");
  assert (not (need_decoding "qwexn--werw.sdfsf"));
  begin
    try
      let (_:string) = decode_domain "xn----7sbksbihemjgbjxflp8bn1jxc.xn--p1aiaudio_orlov_yum" in
      assert false
    with
    | Bad_input -> assert true
    | _ -> assert false
  end;
  ()

let () = self_test ()

end
