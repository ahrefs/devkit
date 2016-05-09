
# 1 "devkit_ragel.ml.rl"


# 6 "devkit_ragel.ml"
let _ipv4_trans_keys : int array = Array.concat [ [|
	0; 0; 48; 57; 46; 57; 48; 57; 46; 57; 48; 57; 46; 57; 48; 57; 
	46; 57; 46; 46; 46; 57; 46; 46; 46; 57; 46; 46; 48; 57; 48; 57; 
	0; 0; 0
|] ]

let _ipv4_key_spans : int array = Array.concat [ [|
	0; 10; 12; 10; 12; 10; 12; 10; 
	12; 1; 12; 1; 12; 1; 10; 10; 
	0
|] ]

let _ipv4_index_offsets : int array = Array.concat [ [|
	0; 0; 11; 24; 35; 48; 59; 72; 
	83; 96; 98; 111; 113; 126; 128; 139; 
	150
|] ]

let _ipv4_indicies : int array = Array.concat [ [|
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
|] ]

let _ipv4_trans_targs : int array = Array.concat [ [|
	2; 0; 3; 12; 4; 5; 10; 6; 
	7; 8; 14; 9; 11; 13; 15; 16
|] ]

let _ipv4_trans_actions : int array = Array.concat [ [|
	1; 0; 2; 3; 1; 4; 3; 1; 
	5; 3; 1; 3; 3; 3; 3; 3
|] ]

let _ipv4_eof_actions : int array = Array.concat [ [|
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 6; 6; 
	6
|] ]

let ipv4_start : int = 1
let ipv4_first_final : int = 14
let ipv4_error : int = 0

let ipv4_en_main : int = 1

type _ipv4_state = { mutable keys : int; mutable trans : int; }
exception Goto_match_ipv4
exception Goto_again_ipv4
exception Goto_eof_trans_ipv4

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

	with Goto_match_ipv4 -> () end;
	do_eof_trans ()
and do_eof_trans () =
	cs.contents <- _ipv4_trans_targs.(state.trans);

	begin try if _ipv4_trans_actions.(state.trans) = 0 then
		raise Goto_again_ipv4;

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
	with Goto_again_ipv4 -> () end;

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
	with Goto_again_ipv4 -> do_again ()
	| Goto_eof_trans_ipv4 -> do_eof_trans () end

	and do_out () = ()
	in do_start ()
	end;

# 19 "devkit_ragel.ml.rl"
  if !cs >= ipv4_first_final then !ip else invalid_arg "parse_ipv4"

let is_ipv4_slow data =
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

	with Goto_match_ipv4 -> () end;
	do_eof_trans ()
and do_eof_trans () =
	cs.contents <- _ipv4_trans_targs.(state.trans);

	begin try if _ipv4_trans_actions.(state.trans) = 0 then
		raise Goto_again_ipv4;

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
	with Goto_again_ipv4 -> () end;

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
	with Goto_again_ipv4 -> do_again ()
	| Goto_eof_trans_ipv4 -> do_eof_trans () end

	and do_out () = ()
	in do_start ()
	end;

# 28 "devkit_ragel.ml.rl"
  !cs >= ipv4_first_final
  with Not_found -> false


# 283 "devkit_ragel.ml"
let _is_ipv4_trans_keys : int array = Array.concat [ [|
	0; 0; 48; 57; 46; 57; 48; 57; 46; 57; 48; 57; 46; 57; 48; 57; 
	46; 57; 46; 46; 46; 57; 46; 53; 46; 57; 46; 46; 46; 57; 46; 53; 
	46; 57; 46; 46; 46; 57; 46; 53; 48; 57; 48; 57; 0; 0; 48; 57; 
	48; 53; 0
|] ]

let _is_ipv4_key_spans : int array = Array.concat [ [|
	0; 10; 12; 10; 12; 10; 12; 10; 
	12; 1; 12; 8; 12; 1; 12; 8; 
	12; 1; 12; 8; 10; 10; 0; 10; 
	6
|] ]

let _is_ipv4_index_offsets : int array = Array.concat [ [|
	0; 0; 11; 24; 35; 48; 59; 72; 
	83; 96; 98; 111; 120; 133; 135; 148; 
	157; 170; 172; 185; 194; 205; 216; 217; 
	228
|] ]

let _is_ipv4_indicies : int array = Array.concat [ [|
	0; 0; 2; 3; 3; 3; 3; 
	3; 3; 3; 1; 4; 1; 3; 3; 
	3; 3; 3; 3; 3; 3; 3; 3; 
	1; 5; 5; 6; 7; 7; 7; 7; 
	7; 7; 7; 1; 8; 1; 7; 7; 
	7; 7; 7; 7; 7; 7; 7; 7; 
	1; 9; 9; 10; 11; 11; 11; 11; 
	11; 11; 11; 1; 12; 1; 11; 11; 
	11; 11; 11; 11; 11; 11; 11; 11; 
	1; 13; 13; 14; 15; 15; 15; 15; 
	15; 15; 15; 1; 12; 1; 16; 16; 
	16; 16; 16; 16; 16; 16; 16; 16; 
	1; 12; 1; 12; 1; 11; 11; 11; 
	11; 11; 17; 16; 16; 16; 16; 1; 
	12; 1; 16; 16; 16; 16; 16; 16; 
	1; 8; 1; 18; 18; 18; 18; 18; 
	18; 18; 18; 18; 18; 1; 8; 1; 
	8; 1; 7; 7; 7; 7; 7; 19; 
	18; 18; 18; 18; 1; 8; 1; 18; 
	18; 18; 18; 18; 18; 1; 4; 1; 
	20; 20; 20; 20; 20; 20; 20; 20; 
	20; 20; 1; 4; 1; 4; 1; 3; 
	3; 3; 3; 3; 21; 20; 20; 20; 
	20; 1; 4; 1; 20; 20; 20; 20; 
	20; 20; 1; 15; 15; 15; 15; 15; 
	15; 15; 15; 15; 15; 1; 22; 22; 
	22; 22; 22; 22; 22; 22; 22; 22; 
	1; 1; 15; 15; 15; 15; 15; 23; 
	22; 22; 22; 22; 1; 22; 22; 22; 
	22; 22; 22; 1; 0
|] ]

let _is_ipv4_trans_targs : int array = Array.concat [ [|
	2; 0; 18; 16; 3; 4; 14; 12; 
	5; 6; 10; 8; 7; 20; 23; 21; 
	9; 11; 13; 15; 17; 19; 22; 24
|] ]

let is_ipv4_start : int = 1
let is_ipv4_first_final : int = 20
let is_ipv4_error : int = 0

let is_ipv4_en_main : int = 1

type _is_ipv4_state = { mutable keys : int; mutable trans : int; }
exception Goto_match_is_ipv4
exception Goto_again_is_ipv4
exception Goto_eof_trans_is_ipv4

# 36 "devkit_ragel.ml.rl"


let is_ipv4 data =
  let cs = ref 0 and p = ref 0 and pe = ref (String.length data) in
  
# 361 "devkit_ragel.ml"
	begin
	cs.contents <- is_ipv4_start;
	end;

# 41 "devkit_ragel.ml.rl"
  
# 368 "devkit_ragel.ml"
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
	let inds = _is_ipv4_index_offsets.(cs.contents) in

	let slen = _is_ipv4_key_spans.(cs.contents) in
	state.trans <- _is_ipv4_indicies.(inds + (
		if slen > 0 && _is_ipv4_trans_keys.(keys) <= Char.code data.[p.contents] &&
		Char.code data.[p.contents] <= _is_ipv4_trans_keys.(keys+1) then
		Char.code data.[p.contents] - _is_ipv4_trans_keys.(keys) else slen));

	with Goto_match_is_ipv4 -> () end;
	do_eof_trans ()
and do_eof_trans () =
	cs.contents <- _is_ipv4_trans_targs.(state.trans);

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
	()
	and do_out () = ()
	in do_start ()
	end;

# 42 "devkit_ragel.ml.rl"
  !cs >= is_ipv4_first_final


# 414 "devkit_ragel.ml"
let _compact_duration_trans_keys : int array = Array.concat [ [|
	0; 0; 48; 57; 46; 115; 48; 115; 48; 115; 48; 115; 115; 115; 0; 0; 
	48; 57; 46; 115; 48; 57; 46; 115; 48; 57; 46; 115; 0
|] ]

let _compact_duration_key_spans : int array = Array.concat [ [|
	0; 10; 70; 68; 68; 68; 1; 0; 
	10; 70; 10; 70; 10; 70
|] ]

let _compact_duration_index_offsets : int array = Array.concat [ [|
	0; 0; 11; 82; 151; 220; 289; 291; 
	292; 303; 374; 385; 456; 467
|] ]

let _compact_duration_indicies : int array = Array.concat [ [|
	0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 1; 2; 1; 3; 3; 
	3; 3; 3; 3; 3; 3; 3; 3; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 4; 1; 1; 1; 5; 1; 
	1; 1; 1; 6; 1; 1; 1; 1; 
	1; 7; 1; 8; 8; 8; 8; 8; 
	8; 8; 8; 8; 8; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 9; 1; 
	10; 10; 10; 10; 10; 10; 10; 10; 
	10; 10; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 7; 1; 11; 11; 11; 
	11; 11; 11; 11; 11; 11; 11; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	7; 1; 7; 1; 1; 12; 12; 12; 
	12; 12; 12; 12; 12; 12; 12; 1; 
	2; 1; 13; 13; 13; 13; 13; 13; 
	13; 13; 13; 13; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 5; 1; 1; 1; 1; 6; 
	1; 1; 1; 1; 1; 7; 1; 14; 
	14; 14; 14; 14; 14; 14; 14; 14; 
	14; 1; 2; 1; 15; 15; 15; 15; 
	15; 15; 15; 15; 15; 15; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 6; 1; 1; 1; 1; 1; 7; 
	1; 16; 16; 16; 16; 16; 16; 16; 
	16; 16; 16; 1; 2; 1; 17; 17; 
	17; 17; 17; 17; 17; 17; 17; 17; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 7; 1; 0
|] ]

let _compact_duration_trans_targs : int array = Array.concat [ [|
	2; 0; 3; 2; 8; 10; 12; 7; 
	4; 7; 5; 6; 9; 9; 11; 11; 
	13; 13
|] ]

let _compact_duration_trans_actions : int array = Array.concat [ [|
	1; 0; 0; 3; 0; 0; 0; 0; 
	5; 6; 7; 7; 9; 3; 11; 3; 
	13; 3
|] ]

let _compact_duration_eof_actions : int array = Array.concat [ [|
	0; 0; 2; 4; 2; 2; 2; 2; 
	8; 2; 10; 2; 12; 2
|] ]

let compact_duration_start : int = 1
let compact_duration_first_final : int = 1
let compact_duration_error : int = 0

let compact_duration_en_main : int = 1

type _compact_duration_state = { mutable keys : int; mutable trans : int; }
exception Goto_match_compact_duration
exception Goto_again_compact_duration
exception Goto_eof_trans_compact_duration

# 53 "devkit_ragel.ml.rl"


let parse_compact_duration data =
  if data = "" then invalid_arg "parse_compact_duration: empty";
  let cs = ref 0 and p = ref 0 and pe = ref (String.length data) and eof = ref (String.length data) in
  let n = ref 0 and f = ref 0. and fn = ref 0. in
  let t = ref 0 in
  
# 538 "devkit_ragel.ml"
	begin
	cs.contents <- compact_duration_start;
	end;

# 61 "devkit_ragel.ml.rl"
  
# 545 "devkit_ragel.ml"
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

	with Goto_match_compact_duration -> () end;
	do_eof_trans ()
and do_eof_trans () =
	cs.contents <- _compact_duration_trans_targs.(state.trans);

	begin try if _compact_duration_trans_actions.(state.trans) = 0 then
		raise Goto_again_compact_duration;

	match _compact_duration_trans_actions.(state.trans) with
	| 3 ->
# 46 "devkit_ragel.ml.rl"
		begin  n := 10 * !n + (Char.code data.[p.contents] - Char.code '0')  end;
	()
	| 6 ->
# 47 "devkit_ragel.ml.rl"
		begin  f := 0.; fn := 1.;  end;
	()
	| 7 ->
# 47 "devkit_ragel.ml.rl"
		begin  fn := !fn *. 10.; f := !f +. float (Char.code data.[p.contents] - Char.code '0') /. !fn;  end;
	()
	| 1 ->
# 46 "devkit_ragel.ml.rl"
		begin  n := 0;  end;
# 46 "devkit_ragel.ml.rl"
		begin  n := 10 * !n + (Char.code data.[p.contents] - Char.code '0')  end;
	()
	| 5 ->
# 47 "devkit_ragel.ml.rl"
		begin  f := 0.; fn := 1.;  end;
# 47 "devkit_ragel.ml.rl"
		begin  fn := !fn *. 10.; f := !f +. float (Char.code data.[p.contents] - Char.code '0') /. !fn;  end;
	()
	| 9 ->
# 48 "devkit_ragel.ml.rl"
		begin  t := !t + !n*24*60*60;  end;
# 46 "devkit_ragel.ml.rl"
		begin  n := 0;  end;
# 46 "devkit_ragel.ml.rl"
		begin  n := 10 * !n + (Char.code data.[p.contents] - Char.code '0')  end;
	()
	| 11 ->
# 49 "devkit_ragel.ml.rl"
		begin  t := !t + !n*60*60;  end;
# 46 "devkit_ragel.ml.rl"
		begin  n := 0;  end;
# 46 "devkit_ragel.ml.rl"
		begin  n := 10 * !n + (Char.code data.[p.contents] - Char.code '0')  end;
	()
	| 13 ->
# 50 "devkit_ragel.ml.rl"
		begin  t := !t + !n*60;  end;
# 46 "devkit_ragel.ml.rl"
		begin  n := 0;  end;
# 46 "devkit_ragel.ml.rl"
		begin  n := 10 * !n + (Char.code data.[p.contents] - Char.code '0')  end;
	()
# 624 "devkit_ragel.ml"
		| _ -> ()
	with Goto_again_compact_duration -> () end;

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
	| 8 ->
# 48 "devkit_ragel.ml.rl"
		begin  t := !t + !n*24*60*60;  end;
	()
	| 10 ->
# 49 "devkit_ragel.ml.rl"
		begin  t := !t + !n*60*60;  end;
	()
	| 12 ->
# 50 "devkit_ragel.ml.rl"
		begin  t := !t + !n*60;  end;
	()
	| 2 ->
# 51 "devkit_ragel.ml.rl"
		begin  t := !t + !n;  end;
	()
	| 4 ->
# 47 "devkit_ragel.ml.rl"
		begin  f := 0.; fn := 1.;  end;
# 51 "devkit_ragel.ml.rl"
		begin  t := !t + !n;  end;
	()
# 663 "devkit_ragel.ml"
		| _ -> ()
	end
	with Goto_again_compact_duration -> do_again ()
	| Goto_eof_trans_compact_duration -> do_eof_trans () end

	and do_out () = ()
	in do_start ()
	end;

# 62 "devkit_ragel.ml.rl"
  if !cs >= compact_duration_first_final then float !t +. !f else invalid_arg "parse_compact_duration"
