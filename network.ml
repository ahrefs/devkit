open Prelude

type ipv4 = int32
type ipv4_cidr = int32 * int32

let ipv4_null = 0l

let bytes_of_ipv4 addr =
  let a = Int32.to_int & Int32.shift_right_logical (Int32.logand 0xFF000000l addr) 24 in
  let b = Int32.to_int & Int32.shift_right_logical (Int32.logand 0x00FF0000l addr) 16 in
  let c = Int32.to_int & Int32.shift_right_logical (Int32.logand 0x0000FF00l addr) 8 in
  let d = Int32.to_int & Int32.logand 0x000000FFl addr in
  (a,b,c,d)

let string_of_ipv4 addr =
  let (a,b,c,d) = bytes_of_ipv4 addr in
  Printf.sprintf "%u.%u.%u.%u" a b c d

(*
1_500_000
   scanf : allocated     73.0GB, heap         0B, collection 0 979 37360, elapsed 43 secs, 23056.45/sec : ok
ocamllex : allocated     11.2GB, heap         0B, collection 0 33 5718, elapsed 17 secs, 57447.94/sec : ok
   ragel : allocated      6.5GB, heap         0B, collection 0 0 3319, elapsed 8.07 secs, 123908.12/sec : ok
*)
let ipv4_of_string_exn = Devkit_ragel.parse_ipv4

let make_broadcast addr netmask = Int32.logor addr (Int32.lognot netmask)

let cidr_of_string_exn s =
  Scanf.sscanf s "%s@/%u%!" (fun ip len ->
    if len < 0 || len > 32 then Exn.fail "bad cidr %s" s;
    let mask = Int32.lognot (if len = 0 then (-1l) else Int32.pred (Int32.shift_left 1l (32 - len))) in
    ipv4_of_string_exn ip, mask)

let range_of_cidr (cidr,mask) = cidr, make_broadcast cidr mask

let ipv4_matches ip (prefix, mask) = Int32.logand ip mask = prefix

let ipv4_of_string_null s = try ipv4_of_string_exn s with _ -> 0l

let is_ipv4 = Devkit_ragel.is_ipv4

let special_cidr = List.map cidr_of_string_exn [
  "0.0.0.0/8"; (*	Current network (only valid as source address)	RFC 1700 *)
  "10.0.0.0/8"; (* Private network	RFC 1918 *)
  "127.0.0.0/8"; (* Loopback	RFC 5735 *)
  "169.254.0.0/16"; (* Link-Local	RFC 3927 *)
  "172.16.0.0/12"; (* Private network	RFC 1918 *)
  "192.0.0.0/24"; (* Reserved (IANA)	RFC 5735 *)
  "192.0.2.0/24"; (* TEST-NET-1, Documentation and example code	RFC 5735 *)
  "192.88.99.0/24"; (* IPv6 to IPv4 relay	RFC 3068 *)
  "192.168.0.0/16"; (* Private network	RFC 1918 *)
  "198.18.0.0/15"; (* Network benchmark tests	RFC 2544 *)
  "198.51.100.0/24"; (* TEST-NET-2, Documentation and examples	RFC 5737 *)
  "203.0.113.0/24"; (* TEST-NET-3, Documentation and examples	RFC 5737 *)
  "224.0.0.0/4"; (* Multicasts (former Class D network)	RFC 3171 *)
  "240.0.0.0/4"; (* Reserved (former Class E network)	RFC 1700 *)
  "255.255.255.255/32"; (* Broadcast	RFC 919 *)
]

