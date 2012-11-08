
# 1 "devkit_ragel.ml.rl"


# 6 "devkit_ragel.ml"
let _ipv4_trans_keys : int array = [|
	0; 0; 48; 57; 46; 57; 48; 57; 46; 57; 48; 57; 46; 57; 48; 57; 
	46; 57; 46; 46; 46; 57; 46; 46; 46; 57; 46; 46; 48; 57; 48; 57; 
	0; 0; 0
|]

let _ipv4_key_spans : int array = [|
	0; 10; 12; 10; 12; 10; 12; 10; 
	12; 1; 12; 1; 12; 1; 10; 10; 
	0
|]

let _ipv4_index_offsets : int array = [|
	0; 0; 11; 24; 35; 48; 59; 72; 
	83; 96; 98; 111; 113; 126; 128; 139; 
	150
|]

let _ipv4_indicies : int array = [|
	0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 1; 2; 1; 3; 3; 
	3; 3; 3; 3; 3; 3; 3; 3; 
	1; 4; 4; 4; 4; 4; 4; 4; 
	4; 4; 4; 1; 5; 1; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	1; 7; 7; 7; 7; 7; 7; 7; 
	7; 7; 7; 1; 8; 1; 9; 9; 
	9; 9; 9; 9; 9; 9; 9; 9; 
	1; 10; 10; 10; 10; 10; 10; 10; 
	10; 10; 10; 1; 8; 1; 11; 11; 
	11; 11; 11; 11; 11; 11; 11; 11; 
	1; 8; 1; 5; 1; 12; 12; 12; 
	12; 12; 12; 12; 12; 12; 12; 1; 
	5; 1; 2; 1; 13; 13; 13; 13; 
	13; 13; 13; 13; 13; 13; 1; 2; 
	1; 14; 14; 14; 14; 14; 14; 14; 
	14; 14; 14; 1; 15; 15; 15; 15; 
	15; 15; 15; 15; 15; 15; 1; 1; 
	0
|]

let _ipv4_trans_targs : int array = [|
	2; 0; 3; 12; 4; 5; 10; 6; 
	7; 8; 14; 9; 11; 13; 15; 16
|]

let _ipv4_trans_actions : int array = [|
	1; 0; 2; 3; 1; 4; 3; 1; 
	5; 3; 1; 3; 3; 3; 3; 3
|]

let _ipv4_eof_actions : int array = [|
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 6; 6; 
	6
|]

let ipv4_start : int = 1
let ipv4_first_final : int = 14
let ipv4_error : int = 0

let ipv4_en_main : int = 1

type _ipv4_state = { mutable keys : int; mutable trans : int; }
exception Goto_match
exception Goto_again
exception Goto_eof_trans

# 7 "devkit_ragel.ml.rl"


let parse_ipv4 data =
  let cs = ref 0 and p = ref 0 and pe = ref (String.length data) and eof = ref (String.length data) in
  let n = ref 0 in
  let ip = ref 0l in
  let set () =
    if !n > 255 then invalid_arg "parse_ipv4";
    ip := Int32.logor (Int32.shift_left !ip 8) (Int32.of_int !n)
  in
  
# 87 "devkit_ragel.ml"
	begin
	cs.contents <- ipv4_start;
	end;

# 18 "devkit_ragel.ml.rl"
  
# 94 "devkit_ragel.ml"
	begin
	let state = { keys = 0; trans = 0; } in
	let rec do_start () =
	if p.contents = pe.contents then
		do_test_eof ()
	else
	if cs.contents = 0 then
		do_out ()
	else
	do_resume ()
and do_resume () =
	begin try
	let keys = cs.contents lsl 1 in
	let inds = _ipv4_index_offsets.(cs.contents) in

	let slen = _ipv4_key_spans.(cs.contents) in
	state.trans <- _ipv4_indicies.(inds + (
		if slen > 0 && _ipv4_trans_keys.(keys) <= Char.code data.[p.contents] &&
		Char.code data.[p.contents] <= _ipv4_trans_keys.(keys+1) then
		Char.code data.[p.contents] - _ipv4_trans_keys.(keys) else slen));

	with Goto_match -> () end;
	do_eof_trans ()
and do_eof_trans () =
	cs.contents <- _ipv4_trans_targs.(state.trans);

	begin try if _ipv4_trans_actions.(state.trans) = 0 then
		raise Goto_again;

	match _ipv4_trans_actions.(state.trans) with
	| 3 ->
# 4 "devkit_ragel.ml.rl"
		begin  n := 10 * !n + (Char.code data.[p.contents] - Char.code '0')  end;
	()
	| 2 ->
# 5 "devkit_ragel.ml.rl"
		begin  set ()  end;
	()
	| 4 ->
# 5 "devkit_ragel.ml.rl"
		begin  set ()  end;
	()
	| 5 ->
# 5 "devkit_ragel.ml.rl"
		begin  set ()  end;
	()
	| 1 ->
# 4 "devkit_ragel.ml.rl"
		begin  n := 0;  end;
# 4 "devkit_ragel.ml.rl"
		begin  n := 10 * !n + (Char.code data.[p.contents] - Char.code '0')  end;
	()
# 147 "devkit_ragel.ml"
		| _ -> ()
	with Goto_again -> () end;

	do_again ()
	and do_again () =
	match cs.contents with
	| 0 -> do_out ()
	| _ ->
	p.contents <- p.contents + 1;
	if p.contents <> pe.contents then
		do_resume ()
	else do_test_eof ()
and do_test_eof () =
	if p.contents = eof.contents then
	begin try
	begin match _ipv4_eof_actions.(cs.contents) with
	| 6 ->
# 5 "devkit_ragel.ml.rl"
		begin  set ()  end;
	()
# 168 "devkit_ragel.ml"
		| _ -> ()
	end
	with Goto_again -> do_again ()
	| Goto_eof_trans -> do_eof_trans () end

	and do_out () = ()
	in do_start ()
	end;

# 19 "devkit_ragel.ml.rl"
  if !cs >= ipv4_first_final then !ip else invalid_arg "parse_ipv4"

let is_ipv4 data =
  let cs = ref 0 and p = ref 0 and pe = ref (String.length data) and eof = ref (String.length data) in
  let n = ref 0 in
  let set () = if !n > 255 then raise Not_found in
  
# 186 "devkit_ragel.ml"
	begin
	cs.contents <- ipv4_start;
	end;

# 26 "devkit_ragel.ml.rl"
  try
  
# 194 "devkit_ragel.ml"
	begin
	let state = { keys = 0; trans = 0; } in
	let rec do_start () =
	if p.contents = pe.contents then
		do_test_eof ()
	else
	if cs.contents = 0 then
		do_out ()
	else
	do_resume ()
and do_resume () =
	begin try
	let keys = cs.contents lsl 1 in
	let inds = _ipv4_index_offsets.(cs.contents) in

	let slen = _ipv4_key_spans.(cs.contents) in
	state.trans <- _ipv4_indicies.(inds + (
		if slen > 0 && _ipv4_trans_keys.(keys) <= Char.code data.[p.contents] &&
		Char.code data.[p.contents] <= _ipv4_trans_keys.(keys+1) then
		Char.code data.[p.contents] - _ipv4_trans_keys.(keys) else slen));

	with Goto_match -> () end;
	do_eof_trans ()
and do_eof_trans () =
	cs.contents <- _ipv4_trans_targs.(state.trans);

	begin try if _ipv4_trans_actions.(state.trans) = 0 then
		raise Goto_again;

	match _ipv4_trans_actions.(state.trans) with
	| 3 ->
# 4 "devkit_ragel.ml.rl"
		begin  n := 10 * !n + (Char.code data.[p.contents] - Char.code '0')  end;
	()
	| 2 ->
# 5 "devkit_ragel.ml.rl"
		begin  set ()  end;
	()
	| 4 ->
# 5 "devkit_ragel.ml.rl"
		begin  set ()  end;
	()
	| 5 ->
# 5 "devkit_ragel.ml.rl"
		begin  set ()  end;
	()
	| 1 ->
# 4 "devkit_ragel.ml.rl"
		begin  n := 0;  end;
# 4 "devkit_ragel.ml.rl"
		begin  n := 10 * !n + (Char.code data.[p.contents] - Char.code '0')  end;
	()
# 247 "devkit_ragel.ml"
		| _ -> ()
	with Goto_again -> () end;

	do_again ()
	and do_again () =
	match cs.contents with
	| 0 -> do_out ()
	| _ ->
	p.contents <- p.contents + 1;
	if p.contents <> pe.contents then
		do_resume ()
	else do_test_eof ()
and do_test_eof () =
	if p.contents = eof.contents then
	begin try
	begin match _ipv4_eof_actions.(cs.contents) with
	| 6 ->
# 5 "devkit_ragel.ml.rl"
		begin  set ()  end;
	()
# 268 "devkit_ragel.ml"
		| _ -> ()
	end
	with Goto_again -> do_again ()
	| Goto_eof_trans -> do_eof_trans () end

	and do_out () = ()
	in do_start ()
	end;

# 28 "devkit_ragel.ml.rl"
  !cs >= ipv4_first_final
  with Not_found -> false


# 283 "devkit_ragel.ml"
let _compact_duration_trans_keys : int array = [|
	0; 0; 46; 115; 48; 115; 48; 115; 48; 115; 115; 115; 46; 115; 46; 115; 
	46; 115; 48; 57; 0; 0; 48; 57; 48; 57; 48; 57; 0
|]

let _compact_duration_key_spans : int array = [|
	0; 70; 68; 68; 68; 1; 70; 70; 
	70; 10; 0; 10; 10; 10
|]

let _compact_duration_index_offsets : int array = [|
	0; 0; 71; 140; 209; 278; 280; 351; 
	422; 493; 504; 505; 516; 527
|]

let _compact_duration_indicies : int array = [|
	0; 1; 2; 2; 2; 2; 2; 
	2; 2; 2; 2; 2; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 3; 
	1; 1; 1; 4; 1; 1; 1; 1; 
	5; 1; 1; 1; 1; 1; 6; 1; 
	7; 7; 7; 7; 7; 7; 7; 7; 
	7; 7; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 8; 1; 9; 9; 9; 
	9; 9; 9; 9; 9; 9; 9; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	6; 1; 10; 10; 10; 10; 10; 10; 
	10; 10; 10; 10; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 6; 1; 6; 
	1; 0; 1; 11; 11; 11; 11; 11; 
	11; 11; 11; 11; 11; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 4; 1; 1; 1; 1; 
	5; 1; 1; 1; 1; 1; 6; 1; 
	0; 1; 12; 12; 12; 12; 12; 12; 
	12; 12; 12; 12; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 5; 
	1; 1; 1; 1; 1; 6; 1; 0; 
	1; 13; 13; 13; 13; 13; 13; 13; 
	13; 13; 13; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 6; 1; 14; 14; 
	14; 14; 14; 14; 14; 14; 14; 14; 
	1; 1; 15; 15; 15; 15; 15; 15; 
	15; 15; 15; 15; 1; 16; 16; 16; 
	16; 16; 16; 16; 16; 16; 16; 1; 
	17; 17; 17; 17; 17; 17; 17; 17; 
	17; 17; 1; 0
|]

let _compact_duration_trans_targs : int array = [|
	2; 0; 1; 11; 12; 13; 10; 3; 
	10; 4; 5; 6; 7; 8; 1; 6; 
	7; 8
|]

let _compact_duration_trans_actions : int array = [|
	0; 0; 1; 0; 0; 0; 0; 2; 
	3; 4; 4; 1; 1; 1; 5; 8; 
	10; 12
|]

let _compact_duration_eof_actions : int array = [|
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 6; 7; 9; 11
|]

let compact_duration_start : int = 9
let compact_duration_first_final : int = 9
let compact_duration_error : int = 0

let compact_duration_en_main : int = 9

type _compact_duration_state = { mutable keys : int; mutable trans : int; }
exception Goto_match
exception Goto_again
exception Goto_eof_trans

# 40 "devkit_ragel.ml.rl"


let parse_compact_duration data =
  if data = "" then invalid_arg "parse_compact_duration: empty";
  let cs = ref 0 and p = ref 0 and pe = ref (String.length data) and eof = ref (String.length data) in
  let n = ref 0 and f = ref 0. and fn = ref 0. in
  let t = ref 0 in
  
# 407 "devkit_ragel.ml"
	begin
	cs.contents <- compact_duration_start;
	end;

# 48 "devkit_ragel.ml.rl"
  
# 414 "devkit_ragel.ml"
	begin
	let state = { keys = 0; trans = 0; } in
	let rec do_start () =
	if p.contents = pe.contents then
		do_test_eof ()
	else
	if cs.contents = 0 then
		do_out ()
	else
	do_resume ()
and do_resume () =
	begin try
	let keys = cs.contents lsl 1 in
	let inds = _compact_duration_index_offsets.(cs.contents) in

	let slen = _compact_duration_key_spans.(cs.contents) in
	state.trans <- _compact_duration_indicies.(inds + (
		if slen > 0 && _compact_duration_trans_keys.(keys) <= Char.code data.[p.contents] &&
		Char.code data.[p.contents] <= _compact_duration_trans_keys.(keys+1) then
		Char.code data.[p.contents] - _compact_duration_trans_keys.(keys) else slen));

	with Goto_match -> () end;
	do_eof_trans ()
and do_eof_trans () =
	cs.contents <- _compact_duration_trans_targs.(state.trans);

	begin try if _compact_duration_trans_actions.(state.trans) = 0 then
		raise Goto_again;

	match _compact_duration_trans_actions.(state.trans) with
	| 1 ->
# 33 "devkit_ragel.ml.rl"
		begin  n := 10 * !n + (Char.code data.[p.contents] - Char.code '0')  end;
	()
	| 3 ->
# 34 "devkit_ragel.ml.rl"
		begin  f := 0.; fn := 1.;  end;
	()
	| 4 ->
# 34 "devkit_ragel.ml.rl"
		begin  fn := !fn *. 10.; f := !f +. float (Char.code data.[p.contents] - Char.code '0') /. !fn;  end;
	()
	| 5 ->
# 33 "devkit_ragel.ml.rl"
		begin  n := 0;  end;
# 33 "devkit_ragel.ml.rl"
		begin  n := 10 * !n + (Char.code data.[p.contents] - Char.code '0')  end;
	()
	| 2 ->
# 34 "devkit_ragel.ml.rl"
		begin  f := 0.; fn := 1.;  end;
# 34 "devkit_ragel.ml.rl"
		begin  fn := !fn *. 10.; f := !f +. float (Char.code data.[p.contents] - Char.code '0') /. !fn;  end;
	()
	| 8 ->
# 35 "devkit_ragel.ml.rl"
		begin  t := !t + !n*24*60*60;  end;
# 33 "devkit_ragel.ml.rl"
		begin  n := 0;  end;
# 33 "devkit_ragel.ml.rl"
		begin  n := 10 * !n + (Char.code data.[p.contents] - Char.code '0')  end;
	()
	| 10 ->
# 36 "devkit_ragel.ml.rl"
		begin  t := !t + !n*60*60;  end;
# 33 "devkit_ragel.ml.rl"
		begin  n := 0;  end;
# 33 "devkit_ragel.ml.rl"
		begin  n := 10 * !n + (Char.code data.[p.contents] - Char.code '0')  end;
	()
	| 12 ->
# 37 "devkit_ragel.ml.rl"
		begin  t := !t + !n*60;  end;
# 33 "devkit_ragel.ml.rl"
		begin  n := 0;  end;
# 33 "devkit_ragel.ml.rl"
		begin  n := 10 * !n + (Char.code data.[p.contents] - Char.code '0')  end;
	()
# 493 "devkit_ragel.ml"
		| _ -> ()
	with Goto_again -> () end;

	do_again ()
	and do_again () =
	match cs.contents with
	| 0 -> do_out ()
	| _ ->
	p.contents <- p.contents + 1;
	if p.contents <> pe.contents then
		do_resume ()
	else do_test_eof ()
and do_test_eof () =
	if p.contents = eof.contents then
	begin try
	begin match _compact_duration_eof_actions.(cs.contents) with
	| 7 ->
# 35 "devkit_ragel.ml.rl"
		begin  t := !t + !n*24*60*60;  end;
	()
	| 9 ->
# 36 "devkit_ragel.ml.rl"
		begin  t := !t + !n*60*60;  end;
	()
	| 11 ->
# 37 "devkit_ragel.ml.rl"
		begin  t := !t + !n*60;  end;
	()
	| 6 ->
# 38 "devkit_ragel.ml.rl"
		begin  t := !t + !n;  end;
	()
# 526 "devkit_ragel.ml"
		| _ -> ()
	end
	with Goto_again -> do_again ()
	| Goto_eof_trans -> do_eof_trans () end

	and do_out () = ()
	in do_start ()
	end;

# 49 "devkit_ragel.ml.rl"
  if !cs >= compact_duration_first_final then float !t +. !f else invalid_arg "parse_compact_duration"
