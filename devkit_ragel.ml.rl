
%%{
 machine ipv4;
 octet = digit{1,3} >{ n := 0; } ${ n := 10 * !n + (Char.code fc - Char.code '0') } ;
 main := octet %{ set () } '.' octet %{ set () } '.' octet %{ set () } '.' octet %{ set () } ;
 write data;
}%%

exception Parse_ipv4 of string

let parse_ipv4 data =
  let cs = ref 0 and p = ref 0 and pe = ref (String.length data) and eof = ref (String.length data) in
  let n = ref 0 in
  let ip = ref 0l in
  let set () =
    if !n > 255 then raise (Parse_ipv4 data);
    ip := Int32.logor (Int32.shift_left !ip 8) (Int32.of_int !n)
  in
  %%write init;
  %%write exec;
  if !cs >= ipv4_first_final then !ip else raise (Parse_ipv4 data)

let is_ipv4_slow data =
  let cs = ref 0 and p = ref 0 and pe = ref (String.length data) and eof = ref (String.length data) in
  let n = ref 0 in
  let set () = if !n > 255 then raise Not_found in
  %%write init;
  try
  %%write exec;
  !cs >= ipv4_first_final
  with Not_found -> false

%%{
 machine is_ipv4;
 octet = ('2' ([0-4] digit | '5' [0-5]) | [01]? digit{1,2}) ;
 main := octet '.' octet '.' octet '.' octet ;
 write data;
}%%

let is_ipv4 data =
  let cs = ref 0 and p = ref 0 and pe = ref (String.length data) in
  %%write init;
  %%write exec;
  !cs >= is_ipv4_first_final

%%{
 machine compact_duration;
 action second { f := !f +. (float(!fn) /. (10. ** float(!fna))); t := !t + !n; fn := 0; fna := 0; }
 action millisecond { f := !f +. (float(!n) /. 1_000.) +. (float(!fn) /. (1000. *. 10. ** float(!fna))); fn := 0; fna := 0; }
 action nanosecond { f := !f +. float(!n) /. 1_000_000_000.; }
 num = digit+ >{ n := 0; } ${ n := 10 * !n + (Char.code fc - Char.code '0') };
 frac = '.' digit{,3} >{ fn := 0; fna := 0 } ${ fn := 10 * !fn + (Char.code fc - Char.code '0') ; fna := !fna + 1; };
 main :=
   (num 'd' %{ t := !t + !n*24*60*60;} )?
   (num 'h' %{ t := !t + !n*60*60; } )?
   (num 'm' %{ t := !t + !n*60; } )?
   ((num frac?'s' %second )? (num frac? 'm' 's' %millisecond )? (num 'n' 's' %nanosecond )? | (num frac? %second )?);
 write data;
}%%

exception Parse_compact_duration of string

let parse_compact_duration data =
  if data = "" then raise (Parse_compact_duration data);
  let cs = ref 0 and p = ref 0 and pe = ref (String.length data) and eof = ref (String.length data) in
  let n = ref 0 and f = ref 0. and fna = ref 0 and fn = ref 0 in
  let t = ref 0 in
  %%write init;
  %%write exec;
  if !cs >= compact_duration_first_final then float !t +. !f else raise (Parse_compact_duration data);
