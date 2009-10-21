(** gzip IO *)

open Prelude
open Control

let input_ic ic =
  IO.create_in 
    ~read:(fun () -> try Gzip.input_char ic with End_of_file -> raise IO.No_more_input)
    ~input:(fun s o l -> match Gzip.input ic s o l with 0 -> raise IO.No_more_input | n -> n)
    ~close:(fun () -> Gzip.close_in ic)

let output_oc oc =
  IO.create_out
    ~write:(Gzip.output_char oc)
    ~output:(fun s o l -> Gzip.output oc s o l; l)
    ~flush:ignore
    ~close:(fun () -> Gzip.close_out oc)

let output_ch = output_oc $ Gzip.open_out_chan
let input_ch = input_ic $ Gzip.open_in_chan
let input = input_ic $ Gzip.open_in

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

