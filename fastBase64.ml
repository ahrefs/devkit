
exception Invalid_char
exception Invalid_table

let chars = [|
	'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';
	'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'a';'b';'c';'d';'e';'f';
	'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';
	'w';'x';'y';'z';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'+';'/'
|]

let make_decoding_table tbl =
	if Array.length tbl <> 64 then raise Invalid_table;
	let d = Array.make 256 (-1) in
	for i = 0 to 63 do
		Array.unsafe_set d (Char.code (Array.unsafe_get tbl i)) i;
	done;
	d

let inv_chars = make_decoding_table chars

let str_decode ?(tbl=inv_chars) s =
	if Array.length tbl <> 256 then raise Invalid_table;
	let data = ref 0 in
	let count = ref 0 in
  let pos = ref 0 in
	let rec fetch () =
		if !count >= 8 then begin
			count := !count - 8;
			let d = (!data asr !count) land 0xFF in
			Char.unsafe_chr d
		end else
			let c = Char.code (String.unsafe_get s !pos) in
			match Array.unsafe_get tbl c with
      | -1 -> raise Invalid_char
      | c ->
        data := (!data lsl 6) lor c;
        incr pos;
        count := !count + 6;
        fetch ()
	in
  let n = String.length s in
  let len =
    if n < 4 then n * 6 / 8 else
    match s.[n-1], s.[n-2] with
    | '=', '=' -> if n mod 4 <> 0 then raise Invalid_char; (n - 2) * 6 / 8
    | '=', _ -> if n mod 4 <> 0 then raise Invalid_char; (n - 1) * 6 / 8
    | _, _ -> n * 6 / 8
  in
	ExtString.String.init len (fun _ -> fetch ())
