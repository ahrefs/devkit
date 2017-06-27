open ExtLib

module type S = sig
  val item_bits : int
  val pp : int -> string
end

module Make(S: S) = struct
  type t = string
  (* the first rem_bits_width bits store the number of remaining unoccupied bits in the last byte *)
  let rem_bits_width = 3 (* 3 for 8-bit chars *)
  let byte_bits = 1 lsl rem_bits_width
  let rem_bits_mask = (1 lsl rem_bits_width) - 1
  let item_mask = (1 lsl S.item_bits) - 1
  let byte_mask = (1 lsl byte_bits) - 1

  let get_remaining_bits s = Char.code s.[0] land rem_bits_mask

  let iterwhile f s =
    let rec iterwhile' f remainder sb rb i last_i s = (* remainder from the previous byte; starting bit; remaining unoccupied bits; current index in the string; last index *)
      match i, sb + S.item_bits, sb with
      | i, sb', _ when i = last_i && sb' > byte_bits - rb -> true
      | _, sb', sb when sb < 0 -> if f @@ ((Char.code s.[i] lsl -sb) lor remainder) land item_mask then iterwhile' f 0 sb' rb i last_i s else false
      | _, sb', _ when sb' <= byte_bits -> if f @@ (Char.code s.[i] lsr sb) land item_mask then iterwhile' f 0 sb' rb i last_i s else false
      | _ (* sb' > byte_bits *) -> iterwhile' f (Char.code s.[i] lsr sb) (sb - byte_bits) rb (i + 1) last_i s
    in
    iterwhile' f 0 rem_bits_width (get_remaining_bits s) 0 (String.length s - 1) s

  let iter f s = ignore @@ iterwhile (fun x -> f x; true) s
  let exists f s = iterwhile (fun x -> not @@ f x) s
  let fold_left f a s = let a = ref a in iter (fun x -> a := f !a x) s; !a

  let to_list v = List.rev @@ fold_left (fun l x -> x::l) [] v
  let of_list l =
    let s_bits = rem_bits_width + (List.length l) * S.item_bits in
    let s_len = (s_bits + byte_bits - 1) / byte_bits in
    let rb = byte_bits - (s_bits - 1) mod byte_bits - 1 in
    let s = Bytes.make s_len '\x00' in
    Bytes.set s 0 (Char.chr rb);
    ignore @@ List.fold_left begin fun (i,rb) x ->
      assert (x land lnot item_mask = 0);
      match rb - S.item_bits with
      | rb' when rb' >= 0 ->
        Bytes.set s i (Char.chr @@ Char.code (Bytes.get s i) lor (x lsl (byte_bits - rb)));
        (i,rb')
      | rb' ->
        let w = x lsl (byte_bits - rb) in
        Bytes.set s (i+1) (Char.chr @@ w lsr byte_bits);
        Bytes.set s i (Char.chr @@ Char.code (Bytes.get s i) lor (w land byte_mask));
        (i + 1, rb' + byte_bits)
    end (0, byte_bits - rem_bits_width) l;
    Bytes.unsafe_to_string s

  let project = Prelude.id
  let inject = function
    | "" -> Log.main #warn "Bit_struct_list.inject error: empty bit string"; of_list []
    | s -> s

  let pp v = "[" ^ (String.concat "; " @@ List.map (fun x -> "\"" ^ (S.pp x) ^ "\"") @@ to_list v) ^ "]"
end
