let bracket resource destroy k = Std.finally (fun () -> destroy resource) k resource

let wrapped acc result k =
  let r = ref None in
  let () = Std.finally (fun () -> r := Some (result acc)) k acc in
  match !r with
  | None -> assert false
  | Some x -> x

let with_open_in_txt name = bracket (open_in name) close_in_noerr
let with_open_out_txt name = bracket (open_out name) close_out_noerr
let with_open_in_bin name = bracket (open_in_bin name) close_in_noerr
let with_open_out_bin name = bracket (open_out_bin name) close_out_noerr
let with_open_out_temp_file ?temp_dir ~mode = bracket (Filename.open_temp_file ~mode ?temp_dir "dvkt" "tmp") (fun (_,ch) -> close_out_noerr ch)
let with_open_out_temp_bin k = with_open_out_temp_file ~mode:[Open_binary] k
let with_open_out_temp_txt k = with_open_out_temp_file ~mode:[Open_text] k

let wrapped_output io = wrapped io IO.close_out
let wrapped_outs k = wrapped_output (IO.output_string ()) k
let with_input io = bracket io IO.close_in
let with_input_bin name k = with_open_in_bin name (fun ch -> k (IO.input_channel ch))
let with_input_txt name k = with_open_in_txt name (fun ch -> k (IO.input_channel ch))
let with_output io = bracket io IO.close_out
let with_output_bin name k = with_open_out_bin name (fun ch -> bracket (IO.output_channel ch) IO.flush k)
let with_output_txt name k = with_open_out_txt name (fun ch -> bracket (IO.output_channel ch) IO.flush k)

let with_opendir dir = bracket (Unix.opendir dir) Unix.closedir
