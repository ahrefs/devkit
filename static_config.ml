
open Prelude
open Printf
open ExtLib

exception Error of string

let fail fmt = ksprintf (fun s -> raise (Error s)) fmt

module Label : sig
type t = private string
val make : string -> t
end = struct
type t = string
let make s =
  let open Stre.ASCII in
  if s <> "" && is_alpha s.[0] && (List.for_all (fun c -> c = '_' || is_alnum c) & String.explode s) then
    s
  else
    fail "bad label %S" s
end

let make_value v (show : 'a -> string) (load : string -> 'a) =
  object
    val mutable contents = v
    val mutable dirty = false
    method get = contents
    method set y = dirty <- true; contents <- y
    method show = show contents
    method load s = dirty <- true; contents <- load s
    method dirty = dirty
    method reset = dirty <- false; contents <- v
  end

type any_value = < show : string; load : string -> unit; dirty : bool; reset : unit; >
type 'a value = < get : 'a; set : 'a -> unit; dirty : bool; >

type group = { label : Label.t; groups : (Label.t, group) Hashtbl.t; values : (Label.t, any_value) Hashtbl.t; parent : group option; }

let group_name g =
  let rec loop acc g =
    match g.parent with
    | None -> String.concat "." (List.rev acc)
    | Some g' -> loop ((g.label :> string) :: acc) g'
  in
  loop [] g

let value_name g (k:Label.t) =
  match group_name g with
  | "" -> (k:>string)
  | s -> s ^ "." ^ (k:>string)

let make_node show load group label (v : 'a) =
  let label = Label.make label in
  if Hashtbl.mem group.values label then fail "duplicate label %S" (label :> string);
  let v = make_value v show load in
  Hashtbl.replace group.values label (v :> any_value);
  (v :> 'a value)

let int = make_node string_of_int int_of_string
let long = make_node Int64.to_string Int64.of_string
let string = make_node id id
let float = make_node string_of_float float_of_string
let bool = make_node string_of_bool (fun s -> match String.lowercase s with
                                              | "false" | "no" -> false
                                              | "true" | "yes" -> true
                                              | s -> fail "not a boolean : %S" s)

let group parent label =
  let label = Label.make label in
  match Hashtbl.find_option parent.groups label with
  | Some x -> x
  | None ->
    let group = { label = label; parent = Some parent; groups = Hashtbl.create 1; values = Hashtbl.create 1; } in
    Hashtbl.add parent.groups label group;
    group

let new_root () = { parent = None; groups = Hashtbl.create 1; values = Hashtbl.create 1; label = Label.make "whatever"; }

let rec iter f g =
  Hashtbl.iter (fun k v -> f (value_name g k) v) g.values;
  Hashtbl.iter (fun _ g -> iter f g) g.groups

let reset = iter (fun _ v -> v#reset)

let read root s =
  reset root;
  let store k v =
    let rec loop g = function
    | [name] -> Hashtbl.find g.values name
    | x::xs -> loop (Hashtbl.find g.groups x) xs
    | [] -> fail "bad key %S" k
    in
    let o = loop root (List.map Label.make & Stre.nsplitc k '.') in
    o#load v
  in
  let store k v =
    try
      store k v
    with
(*     | Not_found -> prerr_endline (Printf.sprintf "Skipping unknown option : %S = %S" k v) *)
    | exn -> fail "Failed to store option : %S = %S : %s" k v (Exn.to_string exn)
  in
  let io = IO.input_string s in
  let line = ref 0 in
  try while true do
    match Exn.catch IO.read_line io with
    | None -> raise Exit
    | Some s ->
      let s = s ^ "\n" in
      incr line;
    try
      Scanf.sscanf s " #" ()
    with Scanf.Scan_failure _ | End_of_file ->
    try
      Scanf.sscanf s " %!" ()
    with Scanf.Scan_failure _ | End_of_file ->
    try
      Scanf.sscanf s "%s = %s@\n%!" (fun k v -> store k (String.strip v))
    with Scanf.Scan_failure _ | End_of_file ->
    try
      Scanf.sscanf s "%s := %c%s@\n%!" (fun k c tail ->
        let pos = String.index tail c in
        String.iter (function ' ' | '\t' -> () | _ -> fail "extra characters after %C-delimtied value" c)
          (String.slice tail ~first:(pos+1));
        store k (String.slice tail ~last:pos))
    with Scanf.Scan_failure _ | End_of_file ->
    try
      Scanf.sscanf s "%s : %d\n%!" (fun k n ->
        assert (n >= 0);
        let l = List.init (n+1) (fun _ -> incr line; IO.read_line io) in
        store k (String.concat "\n" l))
    with Scanf.Scan_failure _ | End_of_file -> fail "can't parse line"
  done with
  | Exit -> ()
  | exn ->
    let s = match exn with Failure s -> s | Error s -> s | exn -> Exn.to_string exn in
    fail "error at line %d : %s" !line s

let choose_quote s =
  let preferred = [ '"'; '\''; '`'; '|'; '!'; '@'; '#'; '%' ] in
  let ok = Array.make 256 true in
  String.iter (fun c -> ok.(Char.code c) <- false) s;
  try
    Some (List.find (fun c -> ok.(Char.code c)) preferred)
  with
    Not_found -> None

let show ?(all=false) root =
  let iter f = iter (fun name v -> if v#dirty || all then f name v#show) in
  let b = Buffer.create 10 in
  iter begin fun name v ->
    match String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 v with
    | 0 ->
      if String.starts_with v " " || String.ends_with v " " then
        begin match choose_quote v with
        | None -> bprintf b "%s :%d\n%s\n" name 0 v
        | Some c -> bprintf b "%s := %c%s%c\n" name c v c
        end
      else
        bprintf b "%s = %s\n" name v
    | n ->
      bprintf b "%s :%d\n%s\n" name n v
  end root;
  Buffer.contents b

let load root file = reset root; match Exn.catch Std.input_file file with None -> () | Some s -> read root s
let save ?all root file = Files.save_as file (fun ch -> output_string ch (show ?all root))

class base root filename =
object
initializer
  load root filename
method save () = save root filename
method load () = load root filename
end

