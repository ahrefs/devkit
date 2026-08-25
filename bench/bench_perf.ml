open Ocamlnet_lite
open Devkit

let output_file = ref "bench-results.json"
let revision = ref "unknown"
let iterations = ref 100
let warmup_iterations = ref 1
let paths = ref []
let list_only = ref false
let sink = ref 0

let repeat_to_at_least pattern target =
  let pattern_len = String.length pattern in
  if pattern_len = 0 then invalid_arg "repeat_to_at_least";
  let count = max 1 ((target + pattern_len - 1) / pattern_len) in
  let buf = Buffer.create (count * pattern_len) in
  for _ = 1 to count do
    Buffer.add_string buf pattern
  done;
  Buffer.contents buf

let repeat_array_to_at_least pattern target =
  let pattern_len = Array.length pattern in
  if pattern_len = 0 then invalid_arg "repeat_array_to_at_least";
  let count = max 1 ((target + pattern_len - 1) / pattern_len) in
  Array.init (count * pattern_len) (fun i -> pattern.(i mod pattern_len))

let batch_for_length len = max 1 (65536 / max 1 len)

type benchmark_case = {
  name : string;
  input_size : int;
  batch_size : int;
  run_batch : unit -> unit;
}

let make_string_case ~prefix ~profile ~target ~pattern f =
  let input = repeat_to_at_least pattern target in
  let batch_size = batch_for_length (String.length input) in
  let run_batch () =
    let total = ref 0 in
    for _ = 1 to batch_size do
      total := !total + String.length (f input)
    done;
    sink := !sink lxor !total
  in
  {
    name = Printf.sprintf "%s.%s_%d" prefix profile target;
    input_size = String.length input;
    batch_size;
    run_batch;
  }

let make_array_case ~prefix ~profile ~target ~pattern f =
  let input = repeat_array_to_at_least pattern target in
  let batch_size = batch_for_length (Array.length input) in
  let run_batch () =
    let total = ref 0 in
    for _ = 1 to batch_size do
      total := !total + String.length (f input)
    done;
    sink := !sink lxor !total
  in
  {
    name = Printf.sprintf "%s.%s_%d" prefix profile target;
    input_size = Array.length input;
    batch_size;
    run_batch;
  }

let regular_targets = [ 64; 4096; 65536 ]
let conversion_targets = [ 64; 4096; 4999; 5000; 5001; 10000; 10001; 65536 ]

let string_cases ~prefix ~targets profiles f =
  List.concat
    (List.map
       (fun (profile, pattern) ->
         List.map
           (fun target ->
             make_string_case ~prefix ~profile ~target ~pattern f)
           targets)
       profiles)

let array_cases ~prefix ~targets profiles f =
  List.concat
    (List.map
       (fun (profile, pattern) ->
         List.map
           (fun target ->
             make_array_case ~prefix ~profile ~target ~pattern f)
           targets)
       profiles)

let url_profiles =
  [
    ("safe_ascii", "Az09_.!*-safe");
    ("spaces", "word word word ");
    ("sparse_escape", "abcdefghijklmnopqrstuvwxyz0123456789&");
    ("dense_escape", " /%~&=+\000\255");
    ("utf8", "café/世界?x=🙂 y");
  ]

let html_profiles =
  [
    ("safe_ascii", "The quick brown fox 123.");
    ("safe_utf8", "café 世界 🙂 ");
    ("sparse_escape", "abcdefghijklmnopqrstuvwxyz0123456789<");
    ("dense_escape", "<>&\"");
    ("mixed", "café <tag a=\"x\">世界 & 🙂</tag>");
  ]

let conversion_profiles =
  [
    ("ascii", "abcdefghijklmnop");
    ("utf8_2byte", "é");
    ("utf8_4byte", "🙂");
    ("mixed", "ascii-é-世界-🙂-");
  ]

let uarray_profiles =
  [
    ("ascii", [| 0x41 |]);
    ("utf8_2byte", [| 0xe9 |]);
    ("utf8_4byte", [| 0x1f642 |]);
    ("mixed", [| 0x41; 0xe9; 0x4e16; 0x1f642 |]);
  ]

let benchmarks () =
  List.concat
    [
      string_cases ~prefix:"url.plus_true" ~targets:regular_targets
        url_profiles (Netencoding.Url.encode ~plus:true);
      string_cases ~prefix:"url.plus_false" ~targets:regular_targets
        url_profiles (Netencoding.Url.encode ~plus:false);
      string_cases ~prefix:"html" ~targets:regular_targets html_profiles
        Web.htmlencode;
      string_cases ~prefix:"netconversion.convert_utf8"
        ~targets:conversion_targets conversion_profiles
        (Netconversion.convert ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8);
      array_cases ~prefix:"netconversion.ustring_of_uarray"
        ~targets:conversion_targets uarray_profiles
        (Netconversion.ustring_of_uarray `Enc_utf8);
    ]

let has_path_prefix prefix name =
  let prefix_len = String.length prefix in
  String.length name >= prefix_len
  && String.sub name 0 prefix_len = prefix
  && (String.length name = prefix_len || name.[prefix_len] = '.')

let selected case =
  !paths = [] || List.exists (fun path -> has_path_prefix path case.name) !paths

type snapshot = {
  wall : float;
  user : float;
  system : float;
  minor_words : float;
  promoted_words : float;
  major_words : float;
  minor_collections : int;
  major_collections : int;
  compactions : int;
}

let snapshot () =
  let gc = Gc.quick_stat () in
  let minor_words, promoted_words, major_words = Gc.counters () in
  let times = Unix.times () in
  {
    wall = Unix.gettimeofday ();
    user = times.Unix.tms_utime;
    system = times.Unix.tms_stime;
    minor_words;
    promoted_words;
    major_words;
    minor_collections = gc.Gc.minor_collections;
    major_collections = gc.Gc.major_collections;
    compactions = gc.Gc.compactions;
  }

let run_times n f =
  for _ = 1 to n do
    f ()
  done

let json_float n = `Float n
let json_int n = `Int n

let measure case =
  run_times !warmup_iterations case.run_batch;
  Gc.full_major ();
  let before = snapshot () in
  run_times !iterations case.run_batch;
  let after = snapshot () in
  let bytes_per_word = float_of_int (Sys.word_size / 8) in
  let minor_bytes = (after.minor_words -. before.minor_words) *. bytes_per_word in
  let promoted_bytes =
    (after.promoted_words -. before.promoted_words) *. bytes_per_word
  in
  let major_bytes = (after.major_words -. before.major_words) *. bytes_per_word in
  let operations = !iterations * case.batch_size in
  `Assoc
    [
      ("name", `String case.name);
      ("revision", `String !revision);
      ("input_size", json_int case.input_size);
      ("batch_size", json_int case.batch_size);
      ("iterations", json_int !iterations);
      ("operations", json_int operations);
      ("wall_seconds", json_float (after.wall -. before.wall));
      ("user_seconds", json_float (after.user -. before.user));
      ("system_seconds", json_float (after.system -. before.system));
      ("minor_allocated_bytes", json_float minor_bytes);
      ( "major_allocated_bytes_including_promoted",
        json_float major_bytes );
      ("major_allocated_bytes_direct", json_float (major_bytes -. promoted_bytes));
      ("promoted_bytes", json_float promoted_bytes);
      ( "minor_collections",
        json_int (after.minor_collections - before.minor_collections) );
      ( "major_collections",
        json_int (after.major_collections - before.major_collections) );
      ("compactions", json_int (after.compactions - before.compactions));
      ("checksum", json_int !sink);
    ]

let () =
  let options =
    [
      ("--output", Arg.Set_string output_file, "FILE write results as JSON");
      ("--revision", Arg.Set_string revision, "LABEL revision stored in JSON");
      ("--iterations", Arg.Set_int iterations, "N measured batches per benchmark");
      ( "--warmup-iterations",
        Arg.Set_int warmup_iterations,
        "N unmeasured warmup batches per benchmark" );
      ( "--path",
        Arg.String (fun path -> paths := path :: !paths),
        "PREFIX select a benchmark or benchmark group" );
      ("--list", Arg.Set list_only, "list benchmark names without running them");
    ]
  in
  Arg.parse options (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg)))
    "bench_perf [OPTIONS]";
  if !iterations <= 0 then raise (Arg.Bad "--iterations must be positive");
  if !warmup_iterations < 0 then
    raise (Arg.Bad "--warmup-iterations must be non-negative");
  let cases = List.filter selected (benchmarks ()) in
  if cases = [] then raise (Arg.Bad "no benchmarks match the selected paths");
  if !list_only then List.iter (fun case -> print_endline case.name) cases
  else (
    let results =
      List.map
        (fun case ->
          Printf.eprintf "[%s] %s\n%!" !revision case.name;
          measure case)
        cases
    in
    Yojson.Safe.to_file !output_file (`List results);
    Printf.eprintf "wrote %d results to %s\n%!" (List.length results)
      !output_file);
  ignore (Sys.opaque_identity !sink)
