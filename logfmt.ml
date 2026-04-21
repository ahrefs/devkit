
let[@inline] needs_escape c = Char.code c < 0x20 || Char.code c >= 0x7f

type cat = Safe | Has_space | Needs_escape

let categorize s : cat =
  let space = ref false in

  try
    for i=0 to String.length s-1 do
      let c = String.unsafe_get s i in
      if needs_escape c then raise_notrace Exit;
      if c = ' ' then space := true
    done;
    if !space then Has_space else Safe
  with Exit -> Needs_escape

let add_pair buf k v =
  Buffer.add_string buf k;
  Buffer.add_char buf '=';
  match categorize v with
  | Safe -> Buffer.add_string buf v
  | Has_space -> Printf.bprintf buf {|"%s"|} v
  | Needs_escape -> Printf.bprintf buf "%S" v

let rec add_to_buffer buf (pairs:Logger.Pairs.t) : unit =
  match pairs with
  | [] -> ()
  | [k,v] -> add_pair buf k v
  | (k,v) :: pairs -> add_pair buf k v; Buffer.add_char buf ' '; add_to_buffer buf pairs

let to_string pairs = match pairs with
  | [] -> ""
  | _ ->
    let buf = Buffer.create 32 in
    add_to_buffer buf pairs;
    Buffer.contents buf
