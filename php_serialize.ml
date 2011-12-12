(** 
  PHP serialization 
  http://php.net/manual/en/function.serialize.php
*)

open ExtLib

open Prelude

(*


Anatomy of a serialize()'ed value:

 String
 s:size:value;

 Integer
 i:value;

 Boolean
 b:value; (does not store "true" or "false", does store '1' or '0')

 Null
 N;

 Array
 a:size:{key definition;value definition;(repeated per element)}

 Object
 O:strlen(object name):object name:object size:{s:strlen(property name):property name:property definition;(repeated per property)}

 String values are always in double quotes
 Array keys are always integers or strings
    "null => 'value'" equates to 's:0:"";s:5:"value";',
    "true => 'value'" equates to 'i:1;s:5:"value";',
    "false => 'value'" equates to 'i:0;s:5:"value";',
    "array(whatever the contents) => 'value'" equates to an "illegal offset type" warning because you can't use an
    array as a key; however, if you use a variable containing an array as a key, it will equate to 's:5:"Array";s:5:"value";',
     and
    attempting to use an object as a key will result in the same behavior as using an array will.
*)

type php = AI of (int * php) list | AS of (string * php) list | S of string | I of int | B of bool | F of float | N |
           EI of (int * php) Enum.t | ES of (string * php) Enum.t | RS of (string * php) list | RI of php list

let check x y = if x <> y then failwith (Printf.sprintf "Php_serialize failed : %u <> %u" x y)

let rec parse_one = parser
  | [< ''a'; '':'; n=number; '':'; ''{'; a=parse_array; ''}' >] -> ignore n;(*check n (List.length a);*) a
  | [< ''b'; '':'; n=number; '';' >] -> B (0 <> n)
  | [< ''d'; '':'; f=parse_float_semi; >] -> F f 
  | [< n=parse_int >] -> I n
  | [< s=parse_str >] -> S s
  | [< ''N'; '';' >] -> N
and number t = parse_nat 0 t
and parse_nat n = parser (* overflow test?* *)
  | [< ''0'..'9' as c; t >] -> let digit = Char.code c - Char.code '0' in parse_nat (n * 10 + digit) t
  | [< >] -> n
and integer = parser
  | [< ''-'; t >] -> - (number t)
  | [< t >] -> number t
and parse_int = parser
  | [< ''i'; '':'; n=integer; '';' >] -> n
and parse_float_semi t = (* ugly, because of one look ahead token FIXME *)
  let buf = Scanf.Scanning.from_function (fun () -> Stream.next t) in
  Scanf.bscanf buf "%f;" (fun f -> f)
and parse_str = parser
  | [< ''s'; '':'; n=number; '':'; ''"'; s=take_string n; ''"'; '';' >] -> s
and take_string n t = String.init n (fun _ -> Stream.next t)
and parse_array = parser
  | [< k=parse_int; v=parse_one; a=parse_int_array [k,v] >] -> AI a
  | [< k=parse_str; v=parse_one; a=parse_str_array [k,v] >] -> AS a
  | [< >] -> AI [] (* empty array *)
and parse_int_array acc = parser
  | [< k=parse_int; v=parse_one; t >] -> parse_int_array ((k,v)::acc) t
  | [< >] -> List.rev acc
and parse_str_array acc = parser
  | [< k=parse_str; v=parse_one; t >] -> parse_str_array ((k,v)::acc) t
  | [< >] -> List.rev acc

let stream_show stream =
  let tail = Stream.npeek 10 stream >> List.map (String.make 1) >> String.concat "" in
  Printf.sprintf "Position %u : %s" (Stream.count stream) tail

let parse ?(full=true) stream =
  try 
    let r = parse_one stream in
    if full then Stream.empty stream;
    r
  with 
  | Stream.Error _ | Stream.Failure -> failwith (stream_show stream)

let parse_string = parse $ Stream.of_string

(** Combinators for easy deconstruction *)

exception Error of string

let fail v str = raise (Error (Printf.sprintf "%s : %s" str (Std.dump v)))

let int = function I n -> n | x -> fail x "int"
let str = function S s -> s | x -> fail x "str"
let bool = function B b -> b | x -> fail x "bool"
let real = function F n -> n | x -> fail x "real"

let opt k x = try Some (k x) with Error _ -> None

let values f = function
  | AS a -> List.map (f $ snd) a
  | AI a -> List.map (f $ snd) a
  | x -> fail x "values"

let array f = function
  | AS a -> List.map (fun (k,v) -> k, f v) a
  | x -> fail x "array"

let assoc php name =
  match php with
  | AS a -> List.assoc name a
  | _ -> fail php "assoc"

module Out = struct

(** Combinators to build values of [php] type *)

let str s = S s
let int n = I n

let  array f e = EI (Enum.mapi (fun i x  -> i, f x) e)
let iarray f e = EI (Enum.map (fun (k,v) -> k, f v) e)
let sarray f e = ES (Enum.map (fun (k,v) -> k, f v) e)

(** Serialize php value *)
let output out v =
  let put_enum n f e = IO.printf out "a:%u:{" n; Enum.iter f e; IO.write out '}' in
  let put_enumi n f e = IO.printf out "a:%u:{" n; Enum.iteri f e; IO.write out '}' in
  let put_int n = IO.printf out "i:%i;" n in
  let put_str s = IO.printf out "s:%u:\"%s\";" (String.length s) s in
  let rec put = function
    | AS a -> put_enum (List.length a) (fun (k,v) -> put_str k; put v) (List.enum a)
    | ES e -> put_enum (Enum.count e) (fun (k,v) -> put_str k; put v) e
    | RS a -> put_enum (List.length a) (fun (k,v) -> put_str k; put v) (List.enum & List.rev a)
    | AI a -> put_enum (List.length a) (fun (k,v) -> put_int k; put v) (List.enum a)
    | EI e -> put_enum (Enum.count e) (fun (k,v) -> put_int k; put v) e
    | RI a -> put_enumi (List.length a) (fun i v -> put_int i; put v) (List.enum & List.rev a)
    | I n -> put_int n
    | B b -> IO.printf out "b:%u;" (if b then 1 else 0)
    | F f -> IO.printf out "d:%g;" (if compare nan f = 0 then 0. else f)
    | N -> IO.nwrite out "N;"
    | S s -> put_str s
  in
  put v

end

let to_string v =
  let out = IO.output_string () in
  Out.output out v;
  IO.close_out out

(** Show php value *)
let show out v =
  let pr fmt = IO.printf out fmt in
  let put_enum f e = pr "{\n"; Enum.iter (fun x -> f x; pr "\n") e; pr "}" in
  let put_enumi f e = pr "{\n"; Enum.iteri (fun i x -> f i x; pr "\n") e; pr "}" in
  let rec put = function
    | AS a -> put_enum (fun (k,v) -> pr "%S : " k; put v) (List.enum a)
    | ES e -> put_enum (fun (k,v) -> pr "%S : " k; put v) e
    | RS a -> put_enum (fun (k,v) -> pr "%S : " k; put v) (List.enum & List.rev a)
    | AI a -> put_enum (fun (k,v) -> pr "%d : " k; put v) (List.enum a)
    | EI e -> put_enum (fun (k,v) -> pr "%d : " k; put v) e
    | RI a -> put_enumi (fun i v -> pr "%d : " i; put v) (List.enum & List.rev a)
    | I n -> pr "%d" n
    | B b -> pr "%B" b
    | F f -> pr "%g" f
    | N -> pr "<NULL>"
    | S s -> pr "%S" s
  in
  put v

let show_s v = 
  let out = IO.output_string () in
  show out v;
  IO.close_out out

