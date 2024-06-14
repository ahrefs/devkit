open ExtLib
open Devkit

let max_u32 = 4*1024*1024*1024

let test len =
  let data = String.make len 'a' in
  let oc = Gzip_io.output (IO.output_string ()) in
  IO.nwrite_string oc data;
  let compressed = IO.close_out oc in
  let ic = Gzip_io.input (IO.input_string compressed) in
  let data = IO.read_all ic in
  IO.close_in ic;
  Memory.reclaim ();
  Log.main #info "original length %d compressed length %d uncompressed length %d" len (String.length compressed) (String.length data);
  if len <> String.length data then failwith @@ Printf.sprintf "test %d failed" len;
  (* let io = Gzip_io.output_ch (Out_channel.open_bin "tempfile.gz") in
  IO.nwrite_string io data;
  IO.close_out io; *)
  String.iter (fun c -> if c <> 'a' then failwith @@ Printf.sprintf "test %d failed" len) data

let () =
  [ 0; 1; 1023; 1024; 1025; max_u32-1; max_u32; max_u32+1 ] |> List.iter test
