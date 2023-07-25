
# 1 "htmlStream_ragel.ml.rl"
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


# 28 "htmlStream_ragel.ml"
let _htmlstream_trans_keys : int array = Array.concat [ [|
	10; 60; 10; 60; 0; 122; 10; 10; 10; 122; 10; 45; 10; 45; 10; 45; 
	10; 62; 10; 122; 0; 122; 0; 122; 0; 122; 0; 122; 0; 62; 0; 62; 
	10; 60; 10; 34; 10; 34; 0; 122; 10; 62; 10; 39; 10; 39; 0; 122; 
	0; 122; 0; 122; 0; 122; 0; 62; 0; 62; 0; 62; 10; 34; 10; 34; 
	0; 122; 10; 39; 10; 39; 0; 122; 10; 62; 10; 122; 10; 122; 10; 60; 
	10; 60; 0; 60; 0; 115; 10; 99; 10; 114; 10; 105; 10; 112; 10; 116; 
	0; 62; 10; 10; 10; 60; 10; 60; 0; 60; 0; 115; 10; 116; 10; 121; 
	10; 108; 10; 101; 0; 62; 10; 10; 10; 62; 10; 10; 0
|] ]

let _htmlstream_key_spans : int array = Array.concat [ [|
	51; 51; 123; 1; 113; 36; 36; 36; 
	53; 113; 123; 123; 123; 123; 63; 63; 
	51; 25; 25; 123; 53; 30; 30; 123; 
	123; 123; 123; 63; 63; 63; 25; 25; 
	123; 30; 30; 123; 53; 113; 113; 51; 
	51; 61; 116; 90; 105; 96; 103; 107; 
	63; 1; 51; 51; 61; 116; 107; 112; 
	99; 92; 63; 1; 53; 1
|] ]

let _htmlstream_index_offsets : int array = Array.concat [ [|
	0; 52; 104; 228; 230; 344; 381; 418; 
	455; 509; 623; 747; 871; 995; 1119; 1183; 
	1247; 1299; 1325; 1351; 1475; 1529; 1560; 1591; 
	1715; 1839; 1963; 2087; 2151; 2215; 2279; 2305; 
	2331; 2455; 2486; 2517; 2641; 2695; 2809; 2923; 
	2975; 3027; 3089; 3206; 3297; 3403; 3500; 3604; 
	3712; 3776; 3778; 3830; 3882; 3944; 4061; 4169; 
	4282; 4382; 4475; 4539; 4541; 4595
|] ]

let _htmlstream_indicies : int array = Array.concat [ [|
	1; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 2; 0; 4; 3; 3; 3; 
	3; 3; 3; 3; 3; 3; 3; 3; 
	3; 3; 3; 3; 3; 3; 3; 3; 
	3; 3; 3; 3; 3; 3; 3; 3; 
	3; 3; 3; 3; 3; 3; 3; 3; 
	3; 3; 3; 3; 3; 3; 3; 3; 
	3; 3; 3; 3; 3; 3; 5; 3; 
	7; 7; 7; 7; 7; 7; 7; 7; 
	7; 7; 8; 7; 7; 7; 7; 7; 
	7; 7; 7; 7; 7; 7; 7; 7; 
	7; 7; 7; 7; 7; 7; 7; 7; 
	7; 9; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 10; 6; 11; 
	10; 10; 10; 10; 10; 10; 10; 10; 
	10; 10; 10; 6; 6; 6; 6; 12; 
	6; 10; 10; 10; 10; 10; 10; 10; 
	10; 10; 10; 10; 10; 10; 10; 10; 
	10; 10; 10; 10; 10; 10; 10; 10; 
	10; 10; 10; 6; 6; 6; 6; 10; 
	6; 10; 10; 10; 10; 10; 10; 10; 
	10; 10; 10; 10; 10; 10; 10; 10; 
	10; 10; 10; 10; 10; 10; 10; 10; 
	10; 10; 10; 6; 14; 13; 15; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 16; 6; 6; 17; 17; 17; 17; 
	17; 17; 17; 17; 17; 17; 6; 6; 
	6; 6; 6; 6; 6; 17; 17; 17; 
	17; 17; 17; 17; 17; 17; 17; 17; 
	17; 17; 17; 17; 17; 17; 17; 17; 
	17; 17; 17; 17; 17; 17; 17; 6; 
	6; 6; 6; 6; 6; 17; 17; 17; 
	17; 17; 17; 17; 17; 17; 17; 17; 
	17; 17; 17; 17; 17; 17; 17; 17; 
	17; 17; 17; 17; 17; 17; 17; 6; 
	15; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 18; 6; 19; 18; 18; 
	18; 18; 18; 18; 18; 18; 18; 18; 
	18; 18; 18; 18; 18; 18; 18; 18; 
	18; 18; 18; 18; 18; 18; 18; 18; 
	18; 18; 18; 18; 18; 18; 18; 18; 
	20; 18; 19; 18; 18; 18; 18; 18; 
	18; 18; 18; 18; 18; 18; 18; 18; 
	18; 18; 18; 18; 18; 18; 18; 18; 
	18; 18; 18; 18; 18; 18; 18; 18; 
	18; 18; 18; 18; 18; 21; 18; 19; 
	18; 18; 18; 18; 18; 18; 18; 18; 
	18; 18; 18; 18; 18; 18; 18; 18; 
	18; 18; 18; 18; 18; 18; 18; 18; 
	18; 18; 18; 18; 18; 18; 18; 18; 
	18; 18; 21; 18; 18; 18; 18; 18; 
	18; 18; 18; 18; 18; 18; 18; 18; 
	18; 18; 18; 22; 18; 15; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	23; 6; 6; 23; 23; 23; 23; 23; 
	23; 23; 23; 23; 23; 23; 6; 6; 
	6; 6; 6; 6; 23; 23; 23; 23; 
	23; 23; 23; 23; 23; 23; 23; 23; 
	23; 23; 23; 23; 23; 23; 23; 23; 
	23; 23; 23; 23; 23; 23; 6; 6; 
	6; 6; 23; 6; 23; 23; 23; 23; 
	23; 23; 23; 23; 23; 23; 23; 23; 
	23; 23; 23; 23; 23; 23; 23; 23; 
	23; 23; 23; 23; 23; 23; 6; 24; 
	24; 24; 24; 24; 24; 24; 24; 24; 
	24; 25; 24; 24; 24; 24; 24; 24; 
	24; 24; 24; 24; 24; 24; 24; 24; 
	24; 24; 24; 24; 24; 24; 24; 24; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 23; 6; 6; 23; 
	23; 23; 23; 23; 23; 23; 23; 23; 
	23; 23; 6; 6; 6; 26; 27; 6; 
	23; 23; 23; 23; 23; 23; 23; 23; 
	23; 23; 23; 23; 23; 23; 23; 23; 
	23; 23; 23; 23; 23; 23; 23; 23; 
	23; 23; 6; 6; 6; 6; 23; 6; 
	23; 23; 23; 23; 23; 23; 23; 23; 
	23; 23; 23; 23; 23; 23; 23; 23; 
	23; 23; 23; 23; 23; 23; 23; 23; 
	23; 23; 6; 28; 28; 28; 28; 28; 
	28; 28; 28; 28; 28; 29; 28; 28; 
	28; 28; 28; 28; 28; 28; 28; 28; 
	28; 28; 28; 28; 28; 28; 28; 28; 
	28; 28; 28; 28; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	30; 6; 6; 30; 30; 30; 30; 30; 
	30; 30; 30; 30; 30; 30; 6; 6; 
	6; 31; 32; 6; 30; 30; 30; 30; 
	30; 30; 30; 30; 30; 30; 30; 30; 
	30; 30; 30; 30; 30; 30; 30; 30; 
	30; 30; 30; 30; 30; 30; 6; 6; 
	6; 6; 30; 6; 30; 30; 30; 30; 
	30; 30; 30; 30; 30; 30; 30; 30; 
	30; 30; 30; 30; 30; 30; 30; 30; 
	30; 30; 30; 30; 30; 30; 6; 33; 
	33; 33; 33; 33; 33; 33; 33; 33; 
	33; 34; 33; 33; 33; 33; 33; 33; 
	33; 33; 33; 33; 33; 33; 33; 33; 
	33; 33; 33; 33; 33; 33; 33; 33; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 35; 6; 6; 35; 
	35; 35; 35; 35; 35; 35; 35; 35; 
	35; 35; 6; 6; 36; 37; 38; 6; 
	35; 35; 35; 35; 35; 35; 35; 35; 
	35; 35; 35; 35; 35; 35; 35; 35; 
	35; 35; 35; 35; 35; 35; 35; 35; 
	35; 35; 6; 6; 6; 6; 35; 6; 
	35; 35; 35; 35; 35; 35; 35; 35; 
	35; 35; 35; 35; 35; 35; 35; 35; 
	35; 35; 35; 35; 35; 35; 35; 35; 
	35; 35; 6; 39; 39; 39; 39; 39; 
	39; 39; 39; 39; 39; 40; 39; 39; 
	39; 39; 39; 39; 39; 39; 39; 39; 
	39; 39; 39; 39; 39; 39; 39; 39; 
	39; 39; 39; 39; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	41; 6; 6; 41; 41; 41; 41; 41; 
	41; 41; 41; 41; 41; 41; 6; 6; 
	42; 43; 44; 6; 41; 41; 41; 41; 
	41; 41; 41; 41; 41; 41; 41; 41; 
	41; 41; 41; 41; 41; 41; 41; 41; 
	41; 41; 41; 41; 41; 41; 6; 6; 
	6; 6; 41; 6; 41; 41; 41; 41; 
	41; 41; 41; 41; 41; 41; 41; 41; 
	41; 41; 41; 41; 41; 41; 41; 41; 
	41; 41; 41; 41; 41; 41; 6; 42; 
	42; 42; 42; 42; 42; 42; 42; 42; 
	42; 46; 42; 42; 42; 42; 42; 42; 
	42; 42; 42; 42; 42; 42; 42; 42; 
	42; 42; 42; 42; 42; 42; 42; 42; 
	45; 47; 45; 45; 45; 45; 48; 45; 
	45; 45; 45; 45; 45; 45; 45; 45; 
	45; 45; 45; 45; 45; 45; 45; 45; 
	45; 45; 45; 45; 45; 6; 45; 50; 
	50; 50; 50; 50; 50; 50; 50; 50; 
	50; 51; 50; 50; 50; 50; 50; 50; 
	50; 50; 50; 50; 50; 50; 50; 50; 
	50; 50; 50; 50; 50; 50; 50; 50; 
	49; 6; 49; 49; 49; 49; 6; 49; 
	49; 49; 49; 49; 49; 49; 49; 49; 
	49; 49; 49; 49; 49; 49; 49; 49; 
	49; 49; 49; 49; 49; 52; 49; 54; 
	53; 53; 53; 53; 53; 53; 53; 53; 
	53; 53; 53; 53; 53; 53; 53; 53; 
	53; 53; 53; 53; 53; 53; 53; 53; 
	53; 53; 53; 53; 53; 53; 53; 53; 
	53; 53; 53; 53; 53; 53; 53; 53; 
	53; 53; 53; 53; 53; 53; 53; 53; 
	53; 55; 53; 57; 56; 56; 56; 56; 
	56; 56; 56; 56; 56; 56; 56; 56; 
	56; 56; 56; 56; 56; 56; 56; 56; 
	56; 56; 56; 58; 56; 60; 59; 59; 
	59; 59; 59; 59; 59; 59; 59; 59; 
	59; 59; 59; 59; 59; 59; 59; 59; 
	59; 59; 59; 59; 59; 61; 59; 62; 
	62; 62; 62; 62; 62; 62; 62; 62; 
	62; 63; 62; 62; 62; 62; 62; 62; 
	62; 62; 62; 62; 62; 62; 62; 62; 
	62; 62; 62; 62; 62; 62; 62; 62; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 41; 6; 6; 41; 
	41; 41; 41; 41; 41; 41; 41; 41; 
	41; 41; 6; 6; 6; 43; 44; 6; 
	41; 41; 41; 41; 41; 41; 41; 41; 
	41; 41; 41; 41; 41; 41; 41; 41; 
	41; 41; 41; 41; 41; 41; 41; 41; 
	41; 41; 6; 6; 6; 6; 41; 6; 
	41; 41; 41; 41; 41; 41; 41; 41; 
	41; 41; 41; 41; 41; 41; 41; 41; 
	41; 41; 41; 41; 41; 41; 41; 41; 
	41; 41; 6; 15; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 31; 
	6; 65; 64; 64; 64; 64; 64; 64; 
	64; 64; 64; 64; 64; 64; 64; 64; 
	64; 64; 64; 64; 64; 64; 64; 64; 
	64; 64; 64; 64; 64; 64; 58; 64; 
	67; 66; 66; 66; 66; 66; 66; 66; 
	66; 66; 66; 66; 66; 66; 66; 66; 
	66; 66; 66; 66; 66; 66; 66; 66; 
	66; 66; 66; 66; 66; 61; 66; 68; 
	68; 68; 68; 68; 68; 68; 68; 68; 
	68; 69; 68; 68; 68; 68; 68; 68; 
	68; 68; 68; 68; 68; 68; 68; 68; 
	68; 68; 68; 68; 68; 68; 68; 68; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 70; 6; 71; 70; 
	70; 70; 70; 70; 70; 70; 70; 70; 
	70; 70; 6; 6; 6; 72; 6; 6; 
	70; 70; 70; 70; 70; 70; 70; 70; 
	70; 70; 70; 70; 70; 70; 70; 70; 
	70; 70; 70; 70; 70; 70; 70; 70; 
	70; 70; 6; 6; 6; 6; 70; 6; 
	70; 70; 70; 70; 70; 70; 70; 70; 
	70; 70; 70; 70; 70; 70; 70; 70; 
	70; 70; 70; 70; 70; 70; 70; 70; 
	70; 70; 6; 73; 73; 73; 73; 73; 
	73; 73; 73; 73; 73; 74; 73; 73; 
	73; 73; 73; 73; 73; 73; 73; 73; 
	73; 73; 73; 73; 73; 73; 73; 73; 
	73; 73; 73; 73; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	75; 6; 76; 75; 75; 75; 75; 75; 
	75; 75; 75; 75; 75; 75; 6; 6; 
	6; 77; 6; 6; 75; 75; 75; 75; 
	75; 75; 75; 75; 75; 75; 75; 75; 
	75; 75; 75; 75; 75; 75; 75; 75; 
	75; 75; 75; 75; 75; 75; 6; 6; 
	6; 6; 75; 6; 75; 75; 75; 75; 
	75; 75; 75; 75; 75; 75; 75; 75; 
	75; 75; 75; 75; 75; 75; 75; 75; 
	75; 75; 75; 75; 75; 75; 6; 78; 
	78; 78; 78; 78; 78; 78; 78; 78; 
	78; 79; 78; 78; 78; 78; 78; 78; 
	78; 78; 78; 78; 78; 78; 78; 78; 
	78; 78; 78; 78; 78; 78; 78; 78; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 80; 6; 81; 80; 
	80; 80; 80; 80; 80; 80; 80; 80; 
	80; 80; 6; 6; 82; 83; 6; 6; 
	80; 80; 80; 80; 80; 80; 80; 80; 
	80; 80; 80; 80; 80; 80; 80; 80; 
	80; 80; 80; 80; 80; 80; 80; 80; 
	80; 80; 6; 6; 6; 6; 80; 6; 
	80; 80; 80; 80; 80; 80; 80; 80; 
	80; 80; 80; 80; 80; 80; 80; 80; 
	80; 80; 80; 80; 80; 80; 80; 80; 
	80; 80; 6; 84; 84; 84; 84; 84; 
	84; 84; 84; 84; 84; 85; 84; 84; 
	84; 84; 84; 84; 84; 84; 84; 84; 
	84; 84; 84; 84; 84; 84; 84; 84; 
	84; 84; 84; 84; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	86; 6; 87; 86; 86; 86; 86; 86; 
	86; 86; 86; 86; 86; 86; 6; 6; 
	88; 89; 6; 6; 86; 86; 86; 86; 
	86; 86; 86; 86; 86; 86; 86; 86; 
	86; 86; 86; 86; 86; 86; 86; 86; 
	86; 86; 86; 86; 86; 86; 6; 6; 
	6; 6; 86; 6; 86; 86; 86; 86; 
	86; 86; 86; 86; 86; 86; 86; 86; 
	86; 86; 86; 86; 86; 86; 86; 86; 
	86; 86; 86; 86; 86; 86; 6; 76; 
	76; 76; 76; 76; 76; 76; 76; 76; 
	76; 90; 76; 76; 76; 76; 76; 76; 
	76; 76; 76; 76; 76; 76; 76; 76; 
	76; 76; 76; 76; 76; 76; 76; 76; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 91; 6; 88; 
	88; 88; 88; 88; 88; 88; 88; 88; 
	88; 93; 88; 88; 88; 88; 88; 88; 
	88; 88; 88; 88; 88; 88; 88; 88; 
	88; 88; 88; 88; 88; 88; 88; 88; 
	92; 94; 92; 92; 92; 92; 95; 92; 
	92; 92; 92; 92; 92; 92; 92; 92; 
	92; 92; 92; 92; 92; 92; 92; 92; 
	92; 92; 92; 92; 92; 6; 92; 97; 
	97; 97; 97; 97; 97; 97; 97; 97; 
	97; 98; 97; 97; 97; 97; 97; 97; 
	97; 97; 97; 97; 97; 97; 97; 97; 
	97; 97; 97; 97; 97; 97; 97; 97; 
	96; 6; 96; 96; 96; 96; 6; 96; 
	96; 96; 96; 96; 96; 96; 96; 96; 
	96; 96; 96; 96; 96; 96; 96; 96; 
	96; 96; 96; 96; 96; 99; 96; 101; 
	100; 100; 100; 100; 100; 100; 100; 100; 
	100; 100; 100; 100; 100; 100; 100; 100; 
	100; 100; 100; 100; 100; 100; 100; 102; 
	100; 104; 103; 103; 103; 103; 103; 103; 
	103; 103; 103; 103; 103; 103; 103; 103; 
	103; 103; 103; 103; 103; 103; 103; 103; 
	103; 105; 103; 106; 106; 106; 106; 106; 
	106; 106; 106; 106; 106; 107; 106; 106; 
	106; 106; 106; 106; 106; 106; 106; 106; 
	106; 106; 106; 106; 106; 106; 106; 106; 
	106; 106; 106; 106; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	86; 6; 87; 86; 86; 86; 86; 86; 
	86; 86; 86; 86; 86; 86; 6; 6; 
	6; 89; 6; 6; 86; 86; 86; 86; 
	86; 86; 86; 86; 86; 86; 86; 86; 
	86; 86; 86; 86; 86; 86; 86; 86; 
	86; 86; 86; 86; 86; 86; 6; 6; 
	6; 6; 86; 6; 86; 86; 86; 86; 
	86; 86; 86; 86; 86; 86; 86; 86; 
	86; 86; 86; 86; 86; 86; 86; 86; 
	86; 86; 86; 86; 86; 86; 6; 109; 
	108; 108; 108; 108; 108; 108; 108; 108; 
	108; 108; 108; 108; 108; 108; 108; 108; 
	108; 108; 108; 108; 108; 108; 108; 108; 
	108; 108; 108; 108; 102; 108; 111; 110; 
	110; 110; 110; 110; 110; 110; 110; 110; 
	110; 110; 110; 110; 110; 110; 110; 110; 
	110; 110; 110; 110; 110; 110; 110; 110; 
	110; 110; 110; 105; 110; 113; 113; 113; 
	113; 113; 113; 113; 113; 113; 113; 114; 
	113; 113; 113; 113; 113; 113; 113; 113; 
	113; 113; 113; 113; 113; 113; 113; 113; 
	113; 113; 113; 113; 113; 113; 112; 112; 
	112; 112; 112; 112; 112; 112; 112; 112; 
	112; 112; 115; 112; 112; 115; 115; 115; 
	115; 115; 115; 115; 115; 115; 115; 115; 
	112; 112; 112; 116; 112; 112; 115; 115; 
	115; 115; 115; 115; 115; 115; 115; 115; 
	115; 115; 115; 115; 115; 115; 115; 115; 
	115; 115; 115; 115; 115; 115; 115; 115; 
	112; 112; 112; 112; 115; 112; 115; 115; 
	115; 115; 115; 115; 115; 115; 115; 115; 
	115; 115; 115; 115; 115; 115; 115; 115; 
	115; 115; 115; 115; 115; 115; 115; 115; 
	112; 118; 117; 117; 117; 117; 117; 117; 
	117; 117; 117; 117; 117; 117; 117; 117; 
	117; 117; 117; 117; 117; 117; 117; 117; 
	117; 117; 117; 117; 117; 117; 117; 117; 
	117; 117; 117; 117; 117; 117; 117; 117; 
	117; 117; 117; 117; 117; 117; 117; 117; 
	117; 117; 117; 117; 117; 119; 117; 121; 
	120; 120; 120; 120; 120; 120; 120; 120; 
	120; 120; 120; 120; 120; 120; 120; 120; 
	120; 120; 120; 120; 120; 120; 120; 120; 
	120; 120; 120; 120; 120; 120; 120; 120; 
	120; 120; 122; 120; 120; 122; 122; 122; 
	122; 122; 122; 122; 122; 122; 122; 122; 
	120; 120; 120; 123; 120; 120; 122; 122; 
	122; 122; 122; 122; 122; 122; 122; 122; 
	122; 122; 122; 122; 122; 122; 122; 122; 
	122; 122; 122; 122; 122; 122; 122; 122; 
	120; 120; 120; 120; 122; 120; 122; 122; 
	122; 122; 122; 122; 122; 122; 122; 122; 
	122; 122; 122; 122; 122; 122; 122; 122; 
	122; 122; 122; 122; 122; 122; 122; 122; 
	120; 15; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 6; 
	6; 6; 6; 6; 6; 6; 6; 17; 
	17; 17; 17; 17; 17; 17; 17; 17; 
	17; 6; 6; 6; 6; 6; 6; 6; 
	17; 17; 17; 17; 17; 17; 17; 17; 
	17; 17; 17; 17; 17; 17; 17; 17; 
	17; 17; 17; 17; 17; 17; 17; 17; 
	17; 17; 6; 6; 6; 6; 6; 6; 
	17; 17; 17; 17; 17; 17; 17; 17; 
	17; 17; 17; 17; 17; 17; 17; 17; 
	17; 17; 17; 17; 17; 17; 17; 17; 
	17; 17; 6; 125; 124; 124; 124; 124; 
	124; 124; 124; 124; 124; 124; 124; 124; 
	124; 124; 124; 124; 124; 124; 124; 124; 
	124; 124; 124; 124; 124; 124; 124; 124; 
	124; 124; 124; 124; 124; 124; 124; 124; 
	124; 124; 124; 124; 124; 124; 124; 124; 
	124; 124; 124; 124; 124; 126; 124; 128; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 129; 127; 130; 130; 130; 130; 130; 
	130; 130; 130; 130; 130; 131; 130; 130; 
	130; 130; 130; 130; 130; 130; 130; 130; 
	130; 130; 130; 130; 130; 130; 130; 130; 
	130; 130; 130; 130; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 132; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 129; 
	127; 132; 132; 132; 132; 132; 132; 132; 
	132; 132; 132; 133; 132; 132; 132; 132; 
	132; 132; 132; 132; 132; 132; 132; 132; 
	132; 132; 132; 132; 132; 132; 132; 132; 
	132; 132; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 129; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 134; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 134; 127; 128; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	129; 127; 127; 127; 127; 127; 127; 135; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 135; 
	127; 128; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 129; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 136; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 136; 127; 128; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 129; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 137; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 137; 127; 128; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 129; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 138; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 138; 127; 128; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 129; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 139; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 139; 127; 
	139; 139; 139; 139; 139; 139; 139; 139; 
	139; 139; 140; 139; 139; 139; 139; 139; 
	139; 139; 139; 139; 139; 139; 139; 139; 
	139; 139; 139; 139; 139; 139; 139; 139; 
	139; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 127; 127; 127; 127; 
	127; 127; 127; 127; 129; 127; 141; 127; 
	143; 142; 145; 144; 144; 144; 144; 144; 
	144; 144; 144; 144; 144; 144; 144; 144; 
	144; 144; 144; 144; 144; 144; 144; 144; 
	144; 144; 144; 144; 144; 144; 144; 144; 
	144; 144; 144; 144; 144; 144; 144; 144; 
	144; 144; 144; 144; 144; 144; 144; 144; 
	144; 144; 144; 144; 146; 144; 148; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	149; 147; 150; 150; 150; 150; 150; 150; 
	150; 150; 150; 150; 151; 150; 150; 150; 
	150; 150; 150; 150; 150; 150; 150; 150; 
	150; 150; 150; 150; 150; 150; 150; 150; 
	150; 150; 150; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 152; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 149; 147; 
	152; 152; 152; 152; 152; 152; 152; 152; 
	152; 152; 153; 152; 152; 152; 152; 152; 
	152; 152; 152; 152; 152; 152; 152; 152; 
	152; 152; 152; 152; 152; 152; 152; 152; 
	152; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 149; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 154; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 154; 147; 148; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 149; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 155; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 155; 
	147; 148; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 149; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	156; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	156; 147; 148; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 149; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 157; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 157; 147; 148; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	149; 147; 147; 147; 147; 147; 147; 147; 
	147; 158; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 158; 147; 158; 158; 158; 158; 158; 
	158; 158; 158; 158; 158; 159; 158; 158; 
	158; 158; 158; 158; 158; 158; 158; 158; 
	158; 158; 158; 158; 158; 158; 158; 158; 
	158; 158; 158; 158; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 147; 
	147; 147; 147; 147; 147; 147; 147; 149; 
	147; 160; 147; 162; 161; 164; 163; 163; 
	163; 163; 163; 163; 163; 163; 163; 163; 
	163; 163; 163; 163; 163; 163; 163; 163; 
	163; 163; 163; 163; 163; 163; 163; 163; 
	163; 163; 163; 163; 163; 163; 163; 163; 
	163; 163; 163; 163; 163; 163; 163; 163; 
	163; 163; 163; 163; 163; 163; 163; 163; 
	163; 165; 163; 167; 166; 0
|] ]

let _htmlstream_trans_targs : int array = Array.concat [ [|
	1; 1; 2; 1; 1; 2; 3; 2; 
	2; 4; 23; 35; 38; 3; 3; 3; 
	5; 9; 6; 6; 7; 8; 0; 10; 
	11; 11; 16; 20; 11; 11; 12; 16; 
	20; 13; 13; 12; 14; 16; 20; 13; 
	13; 12; 14; 16; 20; 15; 14; 17; 
	21; 15; 11; 11; 16; 1; 1; 2; 
	18; 18; 19; 18; 18; 19; 11; 11; 
	22; 22; 22; 22; 24; 24; 23; 27; 
	0; 24; 24; 25; 27; 0; 26; 26; 
	25; 27; 28; 0; 26; 26; 25; 27; 
	28; 0; 27; 0; 29; 28; 30; 33; 
	29; 24; 24; 0; 31; 31; 32; 31; 
	31; 32; 24; 24; 34; 34; 34; 34; 
	36; 35; 35; 37; 0; 36; 36; 0; 
	36; 36; 37; 0; 40; 40; 41; 40; 
	40; 41; 41; 41; 42; 42; 43; 44; 
	45; 46; 47; 48; 48; 49; 49; 49; 
	51; 51; 52; 51; 51; 52; 52; 52; 
	53; 53; 54; 55; 56; 57; 58; 58; 
	59; 59; 59; 60; 60; 61; 61; 61
|] ]

let _htmlstream_trans_actions : int array = Array.concat [ [|
	1; 2; 3; 0; 5; 6; 7; 0; 
	5; 8; 9; 8; 8; 0; 5; 10; 
	0; 1; 0; 5; 0; 0; 0; 0; 
	11; 12; 11; 11; 0; 5; 1; 0; 
	0; 13; 14; 0; 13; 15; 15; 0; 
	5; 16; 0; 17; 17; 1; 5; 0; 
	0; 0; 18; 19; 18; 21; 22; 23; 
	1; 2; 24; 0; 5; 25; 17; 26; 
	1; 2; 0; 5; 27; 28; 0; 27; 
	29; 0; 5; 1; 0; 30; 13; 14; 
	0; 15; 13; 31; 0; 5; 16; 17; 
	0; 32; 5; 33; 1; 5; 0; 0; 
	0; 18; 19; 34; 1; 2; 24; 0; 
	5; 25; 17; 26; 1; 2; 0; 5; 
	24; 0; 5; 1; 35; 0; 5; 36; 
	25; 37; 0; 38; 1; 39; 24; 0; 
	5; 25; 0; 5; 0; 5; 0; 0; 
	0; 0; 0; 0; 5; 40; 0; 5; 
	1; 39; 24; 0; 5; 25; 0; 5; 
	0; 5; 0; 0; 0; 0; 0; 5; 
	41; 0; 5; 0; 5; 42; 0; 5
|] ]

let _htmlstream_eof_actions : int array = Array.concat [ [|
	0; 4; 7; 0; 7; 7; 7; 7; 
	7; 7; 7; 7; 7; 7; 7; 7; 
	20; 7; 7; 7; 7; 7; 7; 7; 
	7; 7; 7; 7; 7; 7; 7; 7; 
	7; 7; 7; 7; 7; 7; 7; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0
|] ]

let htmlstream_start : int = 0
let htmlstream_first_final : int = 0
let htmlstream_error : int = -1

let htmlstream_en_in_script : int = 39
let htmlstream_en_in_style : int = 50
let htmlstream_en_garbage_tag : int = 60
let htmlstream_en_main : int = 0

type _htmlstream_state = { mutable keys : int; mutable trans : int; }
exception Goto_match_htmlstream
exception Goto_again_htmlstream
exception Goto_eof_trans_htmlstream

# 73 "htmlStream_ragel.ml.rl"


(** scan [data] for html tags and invoke [call] for every element  *)
let parse_with_range ?(ctx=init ()) call data =
  let cs = ref 0 in
  let mark = ref (-1) in
  let mark_end = ref (-1) in
  let tag_start = ref (-1) in 
  let tag = ref "" and key = ref "" and attrs = ref [] and directive = ref "" in
(*  let substr data ofs len = try String.sub data ofs len with exn -> Prelude.printfn "%S %d %d %d" data (String.length data) ofs len; raise exn in *)
  let substr = String.sub in
  
# 726 "htmlStream_ragel.ml"
	begin
	cs.contents <- htmlstream_start;
	end;

# 85 "htmlStream_ragel.ml.rl"
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
    assert (Char.equal (String.get data !p) '>');
    let range = (!tag_start, !p + 1) in
    tag_start := -1;
    range
  in
  
# 752 "htmlStream_ragel.ml"
	begin
	let state = { keys = 0; trans = 0; } in
	let rec do_start () =
	if p.contents = pe.contents then
		do_test_eof ()
	else
	do_resume ()
and do_resume () =
	begin try
	let keys = cs.contents lsl 1 in
	let inds = _htmlstream_index_offsets.(cs.contents) in

	let slen = _htmlstream_key_spans.(cs.contents) in
	state.trans <- _htmlstream_indicies.(inds + (
		if slen > 0 && _htmlstream_trans_keys.(keys) <= Char.code data.[p.contents] &&
		Char.code data.[p.contents] <= _htmlstream_trans_keys.(keys+1) then
		Char.code data.[p.contents] - _htmlstream_trans_keys.(keys) else slen));

	with Goto_match_htmlstream -> () end;
	do_eof_trans ()
and do_eof_trans () =
	cs.contents <- _htmlstream_trans_targs.(state.trans);

	begin try if _htmlstream_trans_actions.(state.trans) = 0 then
		raise_notrace Goto_again_htmlstream;

	match _htmlstream_trans_actions.(state.trans) with
	| 1 ->
# 27 "htmlStream_ragel.ml.rl"
		begin  mark := !p  end;
	()
	| 25 ->
# 28 "htmlStream_ragel.ml.rl"
		begin  mark_end := !p  end;
	()
	| 3 ->
# 29 "htmlStream_ragel.ml.rl"
		begin  tag_start := !p  end;
	()
	| 27 ->
# 30 "htmlStream_ragel.ml.rl"
		begin  let s, _r = sub () in tag := String.lowercase_ascii s; attrs := [];  end;
	()
	| 36 ->
# 31 "htmlStream_ragel.ml.rl"
		begin  let s, _r = sub () in call @@ (Close (String.lowercase_ascii s), tag_range ())  end;
	()
	| 11 ->
# 32 "htmlStream_ragel.ml.rl"
		begin  let s, _r = sub () in directive := (String.lowercase_ascii s); attrs := [];  end;
	()
	| 13 ->
# 34 "htmlStream_ragel.ml.rl"
		begin  let s, _r = sub () in key := String.lowercase_ascii s;  end;
	()
	| 17 ->
# 35 "htmlStream_ragel.ml.rl"
		begin  attrs := (!key, Raw.inject (if !mark < 0 then "" else let s, _r = sub() in s)) :: !attrs  end;
	()
	| 30 ->
# 36 "htmlStream_ragel.ml.rl"
		begin 
    match !tag with
    | "script" -> begin cs.contents <- 39; if true then raise_notrace Goto_again_htmlstream end
    | "style" -> begin cs.contents <- 50; if true then raise_notrace Goto_again_htmlstream end
    | "" -> ()
    | _ -> call @@ (Tag (!tag, List.rev !attrs), tag_range ())
  end;
	()
	| 33 ->
# 43 "htmlStream_ragel.ml.rl"
		begin  let r = tag_range () in call @@ (Tag (!tag, List.rev !attrs), r); if !tag <> "a" then call ((Close !tag), r)  end;
	()
	| 7 ->
# 46 "htmlStream_ragel.ml.rl"
		begin  (*printfn "GARBAGE %S" (current ()); *) p.contents <- p.contents - 1;  begin cs.contents <- 60; if true then raise_notrace Goto_again_htmlstream end end;
	()
	| 5 ->
# 48 "htmlStream_ragel.ml.rl"
		begin  ctx.lnum <- ctx.lnum + 1  end;
	()
	| 8 ->
# 69 "htmlStream_ragel.ml.rl"
		begin  tag := ""  end;
	()
	| 24 ->
# 27 "htmlStream_ragel.ml.rl"
		begin  mark := !p  end;
# 28 "htmlStream_ragel.ml.rl"
		begin  mark_end := !p  end;
	()
	| 2 ->
# 27 "htmlStream_ragel.ml.rl"
		begin  mark := !p  end;
# 48 "htmlStream_ragel.ml.rl"
		begin  ctx.lnum <- ctx.lnum + 1  end;
	()
	| 38 ->
# 28 "htmlStream_ragel.ml.rl"
		begin  mark_end := !p  end;
# 31 "htmlStream_ragel.ml.rl"
		begin  let s, _r = sub () in call @@ (Close (String.lowercase_ascii s), tag_range ())  end;
	()
	| 18 ->
# 28 "htmlStream_ragel.ml.rl"
		begin  mark_end := !p  end;
# 35 "htmlStream_ragel.ml.rl"
		begin  attrs := (!key, Raw.inject (if !mark < 0 then "" else let s, _r = sub() in s)) :: !attrs  end;
	()
	| 37 ->
# 28 "htmlStream_ragel.ml.rl"
		begin  mark_end := !p  end;
# 48 "htmlStream_ragel.ml.rl"
		begin  ctx.lnum <- ctx.lnum + 1  end;
	()
	| 29 ->
# 30 "htmlStream_ragel.ml.rl"
		begin  let s, _r = sub () in tag := String.lowercase_ascii s; attrs := [];  end;
# 36 "htmlStream_ragel.ml.rl"
		begin 
    match !tag with
    | "script" -> begin cs.contents <- 39; if true then raise_notrace Goto_again_htmlstream end
    | "style" -> begin cs.contents <- 50; if true then raise_notrace Goto_again_htmlstream end
    | "" -> ()
    | _ -> call @@ (Tag (!tag, List.rev !attrs), tag_range ())
  end;
	()
	| 28 ->
# 30 "htmlStream_ragel.ml.rl"
		begin  let s, _r = sub () in tag := String.lowercase_ascii s; attrs := [];  end;
# 48 "htmlStream_ragel.ml.rl"
		begin  ctx.lnum <- ctx.lnum + 1  end;
	()
	| 12 ->
# 32 "htmlStream_ragel.ml.rl"
		begin  let s, _r = sub () in directive := (String.lowercase_ascii s); attrs := [];  end;
# 48 "htmlStream_ragel.ml.rl"
		begin  ctx.lnum <- ctx.lnum + 1  end;
	()
	| 6 ->
# 33 "htmlStream_ragel.ml.rl"
		begin  let s, r = sub () in call @@ (Text (Raw.inject s), r)  end;
# 29 "htmlStream_ragel.ml.rl"
		begin  tag_start := !p  end;
	()
	| 15 ->
# 34 "htmlStream_ragel.ml.rl"
		begin  let s, _r = sub () in key := String.lowercase_ascii s;  end;
# 35 "htmlStream_ragel.ml.rl"
		begin  attrs := (!key, Raw.inject (if !mark < 0 then "" else let s, _r = sub() in s)) :: !attrs  end;
	()
	| 14 ->
# 34 "htmlStream_ragel.ml.rl"
		begin  let s, _r = sub () in key := String.lowercase_ascii s;  end;
# 48 "htmlStream_ragel.ml.rl"
		begin  ctx.lnum <- ctx.lnum + 1  end;
	()
	| 16 ->
# 35 "htmlStream_ragel.ml.rl"
		begin  attrs := (!key, Raw.inject (if !mark < 0 then "" else let s, _r = sub() in s)) :: !attrs  end;
# 27 "htmlStream_ragel.ml.rl"
		begin  mark := !p  end;
	()
	| 32 ->
# 35 "htmlStream_ragel.ml.rl"
		begin  attrs := (!key, Raw.inject (if !mark < 0 then "" else let s, _r = sub() in s)) :: !attrs  end;
# 36 "htmlStream_ragel.ml.rl"
		begin 
    match !tag with
    | "script" -> begin cs.contents <- 39; if true then raise_notrace Goto_again_htmlstream end
    | "style" -> begin cs.contents <- 50; if true then raise_notrace Goto_again_htmlstream end
    | "" -> ()
    | _ -> call @@ (Tag (!tag, List.rev !attrs), tag_range ())
  end;
	()
	| 26 ->
# 35 "htmlStream_ragel.ml.rl"
		begin  attrs := (!key, Raw.inject (if !mark < 0 then "" else let s, _r = sub() in s)) :: !attrs  end;
# 48 "htmlStream_ragel.ml.rl"
		begin  ctx.lnum <- ctx.lnum + 1  end;
	()
	| 42 ->
# 36 "htmlStream_ragel.ml.rl"
		begin 
    match !tag with
    | "script" -> begin cs.contents <- 39; if true then raise_notrace Goto_again_htmlstream end
    | "style" -> begin cs.contents <- 50; if true then raise_notrace Goto_again_htmlstream end
    | "" -> ()
    | _ -> call @@ (Tag (!tag, List.rev !attrs), tag_range ())
  end;
# 60 "htmlStream_ragel.ml.rl"
		begin  begin cs.contents <- 0; if true then raise_notrace Goto_again_htmlstream end  end;
	()
	| 21 ->
# 44 "htmlStream_ragel.ml.rl"
		begin  (* printfn "directive %s" !directive; *)  end;
# 27 "htmlStream_ragel.ml.rl"
		begin  mark := !p  end;
	()
	| 23 ->
# 44 "htmlStream_ragel.ml.rl"
		begin  (* printfn "directive %s" !directive; *)  end;
# 29 "htmlStream_ragel.ml.rl"
		begin  tag_start := !p  end;
	()
	| 10 ->
# 46 "htmlStream_ragel.ml.rl"
		begin  (*printfn "GARBAGE %S" (current ()); *) p.contents <- p.contents - 1;  begin cs.contents <- 60; if true then raise_notrace Goto_again_htmlstream end end;
# 48 "htmlStream_ragel.ml.rl"
		begin  ctx.lnum <- ctx.lnum + 1  end;
	()
	| 39 ->
# 48 "htmlStream_ragel.ml.rl"
		begin  ctx.lnum <- ctx.lnum + 1  end;
# 27 "htmlStream_ragel.ml.rl"
		begin  mark := !p  end;
	()
	| 40 ->
# 53 "htmlStream_ragel.ml.rl"
		begin 
  let s, _r = sub () in call @@ (Script (List.rev !attrs, s), tag_range ())
  end;
# 55 "htmlStream_ragel.ml.rl"
		begin begin cs.contents <- 0; if true then raise_notrace Goto_again_htmlstream end end;
	()
	| 41 ->
# 56 "htmlStream_ragel.ml.rl"
		begin 
  let s, _r = sub () in call @@ (Style (List.rev !attrs, s), tag_range ())
  end;
# 58 "htmlStream_ragel.ml.rl"
		begin begin cs.contents <- 0; if true then raise_notrace Goto_again_htmlstream end end;
	()
	| 9 ->
# 69 "htmlStream_ragel.ml.rl"
		begin  tag := ""  end;
# 27 "htmlStream_ragel.ml.rl"
		begin  mark := !p  end;
	()
	| 35 ->
# 27 "htmlStream_ragel.ml.rl"
		begin  mark := !p  end;
# 28 "htmlStream_ragel.ml.rl"
		begin  mark_end := !p  end;
# 31 "htmlStream_ragel.ml.rl"
		begin  let s, _r = sub () in call @@ (Close (String.lowercase_ascii s), tag_range ())  end;
	()
	| 34 ->
# 28 "htmlStream_ragel.ml.rl"
		begin  mark_end := !p  end;
# 35 "htmlStream_ragel.ml.rl"
		begin  attrs := (!key, Raw.inject (if !mark < 0 then "" else let s, _r = sub() in s)) :: !attrs  end;
# 36 "htmlStream_ragel.ml.rl"
		begin 
    match !tag with
    | "script" -> begin cs.contents <- 39; if true then raise_notrace Goto_again_htmlstream end
    | "style" -> begin cs.contents <- 50; if true then raise_notrace Goto_again_htmlstream end
    | "" -> ()
    | _ -> call @@ (Tag (!tag, List.rev !attrs), tag_range ())
  end;
	()
	| 19 ->
# 28 "htmlStream_ragel.ml.rl"
		begin  mark_end := !p  end;
# 35 "htmlStream_ragel.ml.rl"
		begin  attrs := (!key, Raw.inject (if !mark < 0 then "" else let s, _r = sub() in s)) :: !attrs  end;
# 48 "htmlStream_ragel.ml.rl"
		begin  ctx.lnum <- ctx.lnum + 1  end;
	()
	| 31 ->
# 34 "htmlStream_ragel.ml.rl"
		begin  let s, _r = sub () in key := String.lowercase_ascii s;  end;
# 35 "htmlStream_ragel.ml.rl"
		begin  attrs := (!key, Raw.inject (if !mark < 0 then "" else let s, _r = sub() in s)) :: !attrs  end;
# 36 "htmlStream_ragel.ml.rl"
		begin 
    match !tag with
    | "script" -> begin cs.contents <- 39; if true then raise_notrace Goto_again_htmlstream end
    | "style" -> begin cs.contents <- 50; if true then raise_notrace Goto_again_htmlstream end
    | "" -> ()
    | _ -> call @@ (Tag (!tag, List.rev !attrs), tag_range ())
  end;
	()
	| 22 ->
# 44 "htmlStream_ragel.ml.rl"
		begin  (* printfn "directive %s" !directive; *)  end;
# 27 "htmlStream_ragel.ml.rl"
		begin  mark := !p  end;
# 48 "htmlStream_ragel.ml.rl"
		begin  ctx.lnum <- ctx.lnum + 1  end;
	()
# 1044 "htmlStream_ragel.ml"
		| _ -> ()
	with Goto_again_htmlstream -> () end;

	do_again ()
	and do_again () =
	p.contents <- p.contents + 1;
	if p.contents <> pe.contents then
		do_resume ()
	else do_test_eof ()
and do_test_eof () =
	if p.contents = eof.contents then
	begin try
	begin match _htmlstream_eof_actions.(cs.contents) with
	| 4 ->
# 33 "htmlStream_ragel.ml.rl"
		begin  let s, r = sub () in call @@ (Text (Raw.inject s), r)  end;
	()
	| 20 ->
# 44 "htmlStream_ragel.ml.rl"
		begin  (* printfn "directive %s" !directive; *)  end;
	()
	| 7 ->
# 46 "htmlStream_ragel.ml.rl"
		begin  (*printfn "GARBAGE %S" (current ()); *) p.contents <- p.contents - 1;  begin cs.contents <- 60; if true then raise_notrace Goto_again_htmlstream end end;
	()
# 1070 "htmlStream_ragel.ml"
		| _ -> ()
	end
	with Goto_again_htmlstream -> do_again ()
	| Goto_eof_trans_htmlstream -> do_eof_trans () end

	in do_start ()
	end;

# 105 "htmlStream_ragel.ml.rl"
(* FIXME ? *)
(*     if !eof <> -1 && !cs < htmlstream_first_final then Exn.fail "not parsed"; *)
  ()

(* vim: ft=ocaml
*)
