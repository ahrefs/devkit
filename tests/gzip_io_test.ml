(* Gzip_io.input: reading lines back from compressed data *)
open Devkit

(* hide any [as_buffered] so that [IO.read_line] goes through [read] one byte at a time *)
let unbuffered inp =
  IO.create_in
    ~read:(fun () -> IO.read inp)
    ~input:(fun buf pos len -> IO.input inp buf pos len)
    ~close:(fun () -> IO.close_in inp)

let by_char z = unbuffered (Gzip_io.input (IO.input_string z))
let current z = Gzip_io.input (IO.input_string z)
let reference raw = unbuffered (IO.input_string raw)

let read_lines inp =
  let rec loop acc =
    match IO.read_line inp with
    | l -> loop (l :: acc)
    | exception IO.No_more_input -> List.rev acc
  in
  let l = loop [] in
  IO.close_in inp;
  l

let failures = ref 0

let check name raw =
  let z = Gzip_io.string raw in
  let expected = read_lines (reference raw) in
  let ok impl f =
    let got = f z in
    if got <> expected then begin
      incr failures;
      Printf.printf "FAIL %s [%s]: %d lines expected, got %d\n%!" name impl (List.length expected) (List.length got)
    end
  in
  ok "by_char" (fun z -> read_lines (by_char z));
  ok "current" (fun z -> read_lines (current z));
  Printf.printf "ok   %s (%d bytes, %d lines)\n%!" name (String.length raw) (List.length expected)

let kib = 1024
let block = 64 * kib

let random_text ~seed ~alphabet n =
  let st = Random.State.make [| seed |] in
  String.init n (fun _ -> alphabet.[Random.State.int st (String.length alphabet)])

(* lines of [len_of i] 'x' (with a few distinct chars) joined by '\n' *)
let lines_of ~count len_of =
  let b = Buffer.create (1 lsl 20) in
  for i = 0 to count - 1 do
    let n = len_of i in
    Buffer.add_string b (String.init n (fun j -> Char.chr (Char.code 'a' + ((i + j) mod 26))));
    Buffer.add_char b '\n'
  done;
  Buffer.contents b


(* random mix of IO ops on the gzip input and on a reference over the raw data, results must match *)
let mixed seed =
  let raw = random_text ~seed ~alphabet:"ab\n\r xyz{}\":0123" (3 * block) in
  let z = Gzip_io.string raw in
  let a = current z and b = reference raw in
  let st = Random.State.make [| seed; 42 |] in
  let size () =
    if Random.State.int st 10 = 0 then Random.State.int st (128 * kib + 1) else Random.State.int st 513
  in
  let res f = match f () with v -> Ok v | exception IO.No_more_input -> Error () in
  let fail op = incr failures; Printf.printf "FAIL mixed seed %d: %s differs\n%!" seed op in
  (try
     for _ = 1 to 300 do
       match Random.State.int st 4 with
       | 0 -> if res (fun () -> IO.read a) <> res (fun () -> IO.read b) then (fail "read"; raise Exit)
       | 1 -> if res (fun () -> IO.read_line a) <> res (fun () -> IO.read_line b) then (fail "read_line"; raise Exit)
       | 2 ->
         let n = size () in
         if res (fun () -> IO.nread_string a n) <> res (fun () -> IO.nread_string b n) then (fail "nread_string"; raise Exit)
       | _ ->
         let n = size () in
         let buf = Bytes.create n in
         (match res (fun () -> IO.input a buf 0 n) with
          | Error () -> if res (fun () -> IO.input b (Bytes.create 1) 0 1) <> Error () then (fail "input eof"; raise Exit)
          | Ok 0 when n > 0 -> fail "input returned 0 instead of raising"; raise Exit
          | Ok k ->
            (* [input] may return short, read exactly as much from the reference *)
            (match res (fun () -> IO.really_nread b k) with
             | Ok exp when Bytes.sub_string buf 0 k = Bytes.to_string exp -> ()
             | _ -> fail "input"; raise Exit))
     done
   with Exit -> ());
  IO.close_in a;
  IO.close_in b;
  Printf.printf "ok   mixed ops seed %d\n%!" seed

let closed () =
  let inp = current (Gzip_io.string "abc\ndef\n") in
  ignore (IO.read_line inp : string);
  IO.close_in inp;
  match IO.read_line inp with
  | _ -> incr failures; print_endline "FAIL read_line after close_in returned"
  | exception IO.Input_closed -> print_endline "ok   read_line after close_in"
  | exception e -> incr failures; Printf.printf "FAIL read_line after close_in raised %s\n%!" (Printexc.to_string e)

let () =
  ignore (Unix.alarm 300 : int);
  check "empty" "";
  check "no trailing newline" "hello";
  check "single newline" "\n";
  check "empty lines" "\n\n\na\n\n";
  check "crlf" "a\r\nb\r\n\r\nc";
  check "cr runs" "a\r\r\r\nb\r\r\rc\r\r\r";
  check "line > 64KiB" (String.make (block + 100) 'x' ^ "\nend\n");
  check "line ends at 64KiB" (String.make (block - 1) 'x' ^ "\nend\n");
  check "newline at 64KiB+1" (String.make block 'x' ^ "\nend\n");
  check "cr run across 64KiB" (String.make (block - 10) 'x' ^ String.make 20 '\r' ^ "\ny\n");
  check "small lines (~8 MiB)" (lines_of ~count:600_000 (fun i -> i mod 24));
  check "big lines (~8 MiB)" (lines_of ~count:40 (fun i -> 100 * kib + (i * 37 * kib mod (200 * kib))));
  check "mixed small and big lines" (lines_of ~count:20_000 (fun i -> if i mod 500 = 0 then 3 * block + i else i mod 100));
  for seed = 0 to 2 do
    check (Printf.sprintf "random text %d" seed) (random_text ~seed ~alphabet:"ab\n\r xyz{}\":0123" (5 * block))
  done;
  for seed = 0 to 19 do mixed seed done;
  closed ();
  if !failures > 0 then (Printf.printf "%d failures\n%!" !failures; exit 1)
