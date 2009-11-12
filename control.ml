(** Control flow *)

let bracket resource destroy k = Std.finally (fun () -> destroy resource) k resource

let with_open_in name = bracket (open_in name) close_in_noerr
let with_open_out name = bracket (open_out name) close_out_noerr
let with_open_in_bin name = bracket (open_in_bin name) close_in_noerr
let with_open_out_bin name = bracket (open_out_bin name) close_out_noerr

let with_output io = bracket io IO.close_out
let with_input io = bracket io IO.close_in

let locked mutex f = Mutex.lock mutex; Std.finally (fun () -> Mutex.unlock mutex) f ()

let suppress f x = try f x with _ -> ()

