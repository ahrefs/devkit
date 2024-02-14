open Prelude

let () = assert (Sys.word_size = 64)

(* reexport exception *)
include (Devkit_ragel : sig exception Parse_ipv4 of string end)

type ipv4 = int32
type ipv4_cidr = int32 * int32

let ipv4_null = 0l
let bytes_of_ipv4 addr =
  let a = Int32.to_int @@ Int32.shift_right_logical (Int32.logand 0xFF000000l addr) 24 in
  let b = Int32.to_int @@ Int32.shift_right_logical (Int32.logand 0x00FF0000l addr) 16 in
  let c = Int32.to_int @@ Int32.shift_right_logical (Int32.logand 0x0000FF00l addr) 8 in
  let d = Int32.to_int @@ Int32.logand 0x000000FFl addr in
  (a,b,c,d)

let string_of_ipv4 addr =
  let (a,b,c,d) = bytes_of_ipv4 addr in
  Printf.sprintf "%u.%u.%u.%u" a b c d

let ipv4_of_int32 = id
let int32_of_ipv4 = id

let ipv4_of_int = Int32.of_int
let int_of_ipv4 = Int32.to_int

(*
1_500_000
   scanf : allocated     73.0GB, heap         0B, collection 0 979 37360, elapsed 43 secs, 23056.45/sec : ok
ocamllex : allocated     11.2GB, heap         0B, collection 0 33 5718, elapsed 17 secs, 57447.94/sec : ok
   ragel : allocated      6.5GB, heap         0B, collection 0 0 3319, elapsed 8.07 secs, 123908.12/sec : ok
*)
let ipv4_of_string_exn = Devkit_ragel.parse_ipv4
let ipv4_of_string_null s = try ipv4_of_string_exn s with _ -> 0l
(*
Network.is_ipv4_slow : allocated      8.0GB, heap         0B, collection 0 0 1019, elapsed 1.57 secs, 31.92/sec : ok
     Network.is_ipv4 : allocated      625KB, heap         0B, collection 0 0 0, elapsed 0.2477 secs, 201.87/sec : ok
*)
let is_ipv4_slow = Devkit_ragel.is_ipv4_slow
let is_ipv4 s =
  String.length s >= 7
  && String.length s <= 15
  && Stre.ASCII.is_digit s.[String.length s - 1]
  && Devkit_ragel.is_ipv4 s

let class_c ip = Int32.logand 0xFFFFFF00l ip

module IPv4 = struct
type t = ipv4
let equal = (=)
let compare = Stdlib.compare
let null = ipv4_null
let to_bytes = bytes_of_ipv4
let to_string = string_of_ipv4
let of_string_exn = ipv4_of_string_exn
let of_string_null = ipv4_of_string_null
let of_int32 = ipv4_of_int32
let to_int32 = int32_of_ipv4
let of_int = ipv4_of_int
let to_int = int_of_ipv4
let class_c = class_c
end

let make_broadcast addr netmask = Int32.logor addr (Int32.lognot netmask)

let cidr_of_string_exn s =
  Scanf.sscanf s "%s@/%u%!" (fun ip len ->
    if len < 0 || len > 32 then Exn.fail "bad cidr %s" s;
    let mask = if len = 0 then 0l else Int32.lognot @@ Int32.pred @@ Int32.shift_left 1l (32 - len) in
    let ip = ipv4_of_string_exn ip in
    Int32.logand ip mask, mask)

let cidr_of_string_exn s = try ipv4_of_string_exn s, -1l with Parse_ipv4 _ -> cidr_of_string_exn s

let string_of_cidr (ip,mask) =
  let rec subnet_bits acc n = if Int32.equal n (-1l) then (32 - acc) else subnet_bits (acc + 1) (Int32.shift_right n 1) in
  Printf.sprintf "%s/%s" (string_of_ipv4 ip) (string_of_int @@ subnet_bits 0 (int32_of_ipv4 mask))

let range_of_cidr (ip0,mask) = ip0, make_broadcast ip0 mask

let ipv4_matches ip (prefix, mask) = Int32.logand ip mask = prefix
let prefix_of_cidr = fst

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

let is_ipv4_special ip = List.exists (ipv4_matches ip) special_cidr

let public_ipv4_network_ips () =
  U.getifaddrs () |> List.filter begin fun (_,ip) ->
    match ipv4_of_string_exn ip with
    | exception _ -> false
    | ip -> not @@ is_ipv4_special ip
  end |> List.map (fun (iface,ip) -> iface, Unix.inet_addr_of_string ip) |> List.sort Stdlib.compare

let private_ipv4_network_ips () =
  (* RFC 1918 *)
  let private_net = List.map cidr_of_string_exn [ "10.0.0.0/8"; "172.16.0.0/12"; "192.168.0.0/16"; ] in
  U.getifaddrs () |> List.filter begin fun (_,ip) ->
    match ipv4_of_string_exn ip with
    | exception _ -> false
    | ip -> List.exists (ipv4_matches ip) private_net
  end |> List.map (fun (iface,ip) -> iface, Unix.inet_addr_of_string ip) |> List.sort Stdlib.compare

let private_ipv4_network_ip () =
  match private_ipv4_network_ips () with
  | [] -> Unix.inet_addr_loopback
  | (_,ip)::_ -> ip

let public_ipv4_network_ip () = match public_ipv4_network_ips () with [] -> None | (_,ip)::_ -> Some ip
let public_ipv4_network_ip_exn () = match public_ipv4_network_ip () with None -> Exn.fail "Network: no public ipv4 address" | Some ip -> ip

let public_network_ips = public_ipv4_network_ips
let private_network_ips = private_ipv4_network_ips
let private_network_ip = private_ipv4_network_ip

let ipv4_to_yojson ip = `String (string_of_ipv4 ip)
let ipv4_of_yojson j =
  match j with
  | `String i -> begin try Ok (ipv4_of_string_exn i) with exn -> Error (Printf.sprintf "ipv4: cannot parse %S (%s)" i (Exn.to_string exn)) end
  | _ -> Error "ipv4: expected string"
