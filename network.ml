open Prelude

let bytes_of_ipv4 addr =
  let a = Int32.to_int & Int32.shift_right_logical (Int32.logand 0xFF000000l addr) 24 in
  let b = Int32.to_int & Int32.shift_right_logical (Int32.logand 0x00FF0000l addr) 16 in
  let c = Int32.to_int & Int32.shift_right_logical (Int32.logand 0x0000FF00l addr) 8 in
  let d = Int32.to_int & Int32.logand 0x000000FFl addr in
  (a,b,c,d)

let string_of_ipv4 addr =
  let (a,b,c,d) = bytes_of_ipv4 addr in
  Printf.sprintf "%u.%u.%u.%u" a b c d

let make_ip =
  let byte cout s =
    let x = int_of_string s in
    if x >= 0 && x < 256 then 
      IO.write_byte cout x 
    else 
      Exn.fail "ipv4_of_string: bad octet in %s" s
  in
  fun a b c d ->
    let (cin,cout) = IO.pipe () in
    byte cout d;
    byte cout c;
    byte cout b;
    byte cout a;
    IO.close_out cout;
    let ip = IO.read_real_i32 cin in
    IO.close_in cin;
    ip

let ipv4_of_string_exn s = Scanf.sscanf s "%3[0-9].%3[0-9].%3[0-9].%3[0-9]%!" make_ip

let cidr_of_string_exn s =
  Scanf.sscanf s "%s@/%u%!" (fun ip len ->
    if len < 0 || len > 32 then Exn.fail "bad cidr %s" s;
    let mask = Int32.lognot (if len = 0 then (-1l) else Int32.pred (Int32.shift_left 1l (32 - len))) in
    ipv4_of_string_exn ip, mask)

let ipv4_matches ip (prefix, mask) = Int32.logand ip mask = prefix

let ipv4_of_string_null s = try ipv4_of_string_exn s with _ -> 0l

let is_ipv4 s = 0l <> ipv4_of_string_null s


