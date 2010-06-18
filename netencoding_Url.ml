
module Str = Netstring_pcre

  let hex_digits =
    [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
       '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F' |];;

  let to_hex2 k =
    (* Converts k to a 2-digit hex string *)
    let s = String.create 2 in
    s.[0] <- hex_digits.( (k lsr 4) land 15 );
    s.[1] <- hex_digits.( k land 15 );
    s ;;


  let of_hex1 c =
    match c with
	('0'..'9') -> Char.code c - Char.code '0'
      | ('A'..'F') -> Char.code c - Char.code 'A' + 10
      | ('a'..'f') -> Char.code c - Char.code 'a' + 10
      | _ ->
	raise Not_found ;;



  let url_encoding_re =
    Str.regexp "[^A-Za-z0-9_.!*-]";;

  let url_decoding_re =
    Str.regexp "\\+|%..|%.|%";;


  let encode ?(plus = true) s =
    Str.global_substitute
      url_encoding_re
      (fun r _ ->
	 match Str.matched_string r s with
	     " " when plus -> "+"
	   | x ->
	       let k = Char.code(x.[0]) in
	       "%" ^ to_hex2 k
      )
      s ;;


  let decode ?(plus = true) s =
    let l = String.length s in
    Str.global_substitute
      url_decoding_re
      (fun r _ ->
	 match Str.matched_string r s with
	   | "+" -> if plus then " " else "+"
	   | _ ->
	       let i = Str.match_beginning r in
	       (* Assertion: s.[i] = '%' *)
	       if i+2 >= l then failwith "Netencoding.Url.decode";
	       let c1 = s.[i+1] in
	       let c2 = s.[i+2] in
	       begin
		 try
		   let k1 = of_hex1 c1 in
		   let k2 = of_hex1 c2 in
		   String.make 1 (Char.chr((k1 lsl 4) lor k2))
		 with
		     Not_found ->
		       failwith "Netencoding.Url.decode"
	       end
      )
      s ;;

  let url_split_re =
    Str.regexp "[&=]";;

  let mk_url_encoded_parameters nv_pairs =
    String.concat "&"
      (List.map
	 (fun (name,value) ->
	    let name_encoded = encode name in
	    let value_encoded = encode value in
	    name_encoded ^ "=" ^ value_encoded
	 )
	 nv_pairs
      )
  ;;

  let dest_url_encoded_parameters parstr =
    let rec parse_after_amp tl =
      match tl with
	  Str.Text name :: Str.Delim "=" :: Str.Text value :: tl' ->
	    (decode name, decode value) :: parse_next tl'
	| Str.Text name :: Str.Delim "=" :: Str.Delim "&" :: tl' ->
	    (decode name, "") :: parse_after_amp tl'
	| Str.Text name :: Str.Delim "=" :: [] ->
	    [decode name, ""]
	| _ ->
	    failwith "Netencoding.Url.dest_url_encoded_parameters"
    and parse_next tl =
      match tl with
	  [] -> []
	| Str.Delim "&" :: tl' ->
	    parse_after_amp tl'
	| _ ->
	    failwith "Netencoding.Url.dest_url_encoded_parameters"
    in
    let toklist = Str.full_split url_split_re parstr in
    match toklist with
	[] -> []
      | _ -> parse_after_amp toklist
  ;;
