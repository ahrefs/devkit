(** gzip IO *)

let input io =
  let iz = Gzip_stream.open_in io in
  IO.create_in 
    ~read:(fun () -> Gzip_stream.input_char iz)
    ~input:(Gzip_stream.input iz)
    ~close:(fun () -> Gzip_stream.close_in iz)

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
  IO.nwrite out s;
  IO.close_out out
