(** gzip IO *)

let buf_size = 65536

let input io =
  let stream = Gzip_stream.open_in io in
  (* Gzip_stream.input returns 0 at end of stream, which [Buf_in.create] turns into [No_more_input] *)
  let buf_in = IO.Buf_in.create (Bytes.create buf_size) ~fill:(fun buf -> Gzip_stream.input stream buf 0 (Bytes.length buf)) in
  let read () =
    IO.Buf_in.refill_if_empty buf_in;
    assert (buf_in.len > 0);
    let c = Bytes.unsafe_get buf_in.buf buf_in.pos in
    IO.Buf_in.consume buf_in 1;
    c
  in
  let input buf pos len =
    if buf_in.len = 0 then (
      (* passthrough *)
      let n = Gzip_stream.input stream buf pos len in
      if n = 0 && len > 0 then raise IO.No_more_input;
      n
    )
    else (
      let n = min len buf_in.len in
      Bytes.blit buf_in.buf buf_in.pos buf pos n;
      IO.Buf_in.consume buf_in n;
      n
    )
  in
  let buffered = Some buf_in in
  IO.create_in_full ()
    ~read
    ~input
    ~close:(fun () -> Gzip_stream.close_in stream)
    ~as_buffered:(fun () -> buffered)

let output io =
  let oz = Gzip_stream.open_out io in
  IO.create_out
    ~write:(Gzip_stream.output_char oz)
    ~output:(fun s o l -> Gzip_stream.output oz s o l; l)
    ~flush:(fun () -> IO.flush io)
    ~close:(fun () -> Gzip_stream.close_out oz)

let input_ch ch = input (IO.input_channel ch)
let output_ch ch = output (IO.output_channel ch)

(*
let pipe_in f =
  bracket (Filename.open_temp_file ~mode:[Open_binary] "gzip_io" "gz")
    (fun (tmpname,ch) -> close_out_noerr ch; Sys.remove tmpname)
    (fun (tmpname,ch) ->
      bracket (output_ch ch) (suppress IO.close_out) (fun out ->
        f out;
        IO.close_out out;
        Std.input_file ~bin:true tmpname
      )
    )
*)

let string s =
  let out = output (IO.output_string ()) in
  IO.nwrite out (Bytes.unsafe_of_string s); (* IO wrong type *)
  IO.close_out out

let to_string s =
  let inp = input (IO.input_string s) in
  let out = IO.output_string () in
  try
    while true do
      IO.write out (IO.read inp)
    done;
    assert false
  with IO.No_more_input ->
    IO.close_in inp;
    IO.close_out out
