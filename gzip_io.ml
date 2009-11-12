(** gzip IO *)

open Prelude
open Control

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

(*
let input_ic ic = input (IO.input_channel ic)
let output_oc oc = output (IO.output_channel oc)
*)

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

