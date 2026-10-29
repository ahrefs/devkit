(* Gzip_io.input: IO.read_line throughput, one byte at a time ("by_char") vs
   directly on the channel ("current").

   dune exec --release bench/bench_gzip_io.exe *)

open Devkit

(* hide any [as_buffered] so that [IO.read_line] goes through [read] one byte at a time;
   also returns the number of bytes read so far *)
let unbuffered_counting inp =
  let n = ref 0 in
  ( IO.create_in
      ~read:(fun () ->
        let c = IO.read inp in
        incr n;
        c)
      ~input:(fun buf pos len ->
        let k = IO.input inp buf pos len in
        n := !n + k;
        k)
      ~close:(fun () -> IO.close_in inp),
    fun () -> !n )

(* [current] reads through the buffer directly, so there is no byte count to get *)
let impls =
  [ ( "by_char",
      fun z ->
        let inp, count = unbuffered_counting (Gzip_io.input (IO.input_string z)) in
        inp, Some count );
    "current", fun z -> Gzip_io.input (IO.input_string z), None ]

let lines_of ~count len_of =
  let b = Buffer.create (1 lsl 20) in
  for i = 0 to count - 1 do
    Buffer.add_string b (String.init (len_of i) (fun j -> Char.chr (Char.code 'a' + ((i + j) mod 26))));
    Buffer.add_char b '\n'
  done;
  Buffer.contents b

let kib = 1024

(* each dataset is generated on demand to keep memory low; sized so that [current] runs for ~100ms+ *)
let datasets =
  [ "small lines (~0-100B)", (fun () -> lines_of ~count:3_000_000 (fun i -> i * 7 mod 100));
    "medium lines (~1-4KiB)", (fun () -> lines_of ~count:60_000 (fun i -> kib + (i * 131 mod (3 * kib))));
    "big lines (~100-300KiB)", (fun () -> lines_of ~count:1200 (fun i -> (100 * kib) + (i * 37 * kib mod (200 * kib))));
    "mixed", (fun () -> lines_of ~count:600_000 (fun i -> if i mod 1000 = 0 then 200 * kib else i mod 80)) ]

let time f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  r, Unix.gettimeofday () -. t0

let count_lines inp =
  let n = ref 0 in
  (try
     while true do
       ignore (IO.read_line inp : string);
       incr n
     done
   with IO.No_more_input -> ());
  IO.close_in inp;
  !n

(* just decompress, no parsing: one [IO.read] per byte *)
let through_chars inp =
  let n = ref 0 in
  (try
     while true do
       ignore (IO.read inp : char);
       incr n
     done
   with IO.No_more_input -> ());
  IO.close_in inp;
  !n

(* just decompress, no parsing: [IO.input] into a 64 KiB buffer *)
let through_chunks inp =
  let buf = Bytes.create (64 * kib) in
  let n = ref 0 in
  (try
     while true do
       n := !n + IO.input inp buf 0 (Bytes.length buf)
     done
   with IO.No_more_input -> ());
  IO.close_in inp;
  !n

(* what is done with the input, per implementation; results (line or byte counts) must agree
   across implementations *)
let modes =
  [ "lines", [ "by_char", count_lines; "current", count_lines ];
    "through", [ "by_char", through_chars; "current", through_chunks ] ]

let () =
  ignore (Unix.alarm 900 : int);
  let runs = 3 in
  List.iter (fun (dname, gen) ->
    let raw = gen () in
    let z = Gzip_io.string raw in
    Printf.printf "%s: %s raw, %s gzip\n%!" dname
      (Action.bytes_string (String.length raw)) (Action.bytes_string (String.length z));
    List.iter (fun (mname, consumers) ->
      let expected = ref None in
      List.iter (fun (iname, consume) ->
        let mk = List.assoc iname impls in
        let best = ref infinity in
        for _ = 1 to runs do
          let (result, count), dt = time (fun () ->
            let inp, count = mk z in
            consume inp, count)
          in
          (match !expected with
           | None -> expected := Some result
           | Some e -> if e <> result then failwith (Printf.sprintf "%s/%s/%s: result mismatch" dname mname iname));
          Stdlib.Option.iter (fun count -> if count () <> String.length raw then failwith (Printf.sprintf "%s/%s/%s: byte count mismatch" dname mname iname)) count;
          best := min !best dt
        done;
        Printf.printf "  %-8s %-8s %8.1f MB/s  (%.3fs)\n%!" mname iname (float (String.length raw) /. 1e6 /. !best) !best)
        consumers)
      modes)
    datasets
