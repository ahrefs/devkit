(** Control flow *)

(** [bracker resource destroy k]
    @return [k resource] and guarantee that [resource] is [destroy]'ed at the end. *)
let bracket resource destroy k = Std.finally (fun () -> destroy resource) k resource

(** [wrapped acc result k]

  Computation [k] accumulates result into resource [acc] which
  is guaranteed to be released at the end. Rarely useful (e.g. {!IO.output_string})
  @return [result acc] *)
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
let with_open_out_temp_file ~mode = bracket (Filename.open_temp_file ~mode "dvkt" "tmp") (fun (_,ch) -> close_out_noerr ch)
let with_open_out_temp_bin k = with_open_out_temp_file ~mode:[Open_binary] k

let with_output io = bracket io IO.close_out
let wrapped_output io = wrapped io IO.close_out
let wrapped_outs k = wrapped_output (IO.output_string ()) k
let with_input io = bracket io IO.close_in
let with_input_bin name k = with_open_in_bin name (fun ch -> k (IO.input_channel ch))
let with_output_bin name k = with_open_out_bin name (fun ch -> k (IO.output_channel ch))

let locked mutex f = Mutex.lock mutex; Std.finally (fun () -> Mutex.unlock mutex) f ()

let suppress f x = try f x with _ -> ()

