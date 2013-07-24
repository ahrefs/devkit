(** Misc *)

open ExtLib
open Printf

open Prelude

let period n f = 
  let count = ref 0 in
  (fun () -> incr count; if !count mod n = 0 then f !count)

let strl f l = sprintf "[%s]" (String.concat ";" (List.map f l))
let catmap f l = String.concat "" (List.map f l)

let uniq p e =
  let h = Hashtbl.create 16 in
  Enum.filter (fun x ->
    let k = p x in
    if Hashtbl.mem h k then false else (Hashtbl.add h k (); true)) e

let list_uniq p = List.of_enum $ uniq p $ List.enum

let list_random_exn l = List.nth l (Random.int (List.length l))

let list_random = function
  | [] -> None
  | l -> list_random_exn l >> some

let array_random_exn a = a.(Random.int (Array.length a))
let array_random = function [||] -> None | a -> Some (array_random_exn a)

(** @return index of first element mathing [p] when iterating [a] in reverse
    @raise Not_found if no such element exists *)
let array_rfindi p a =
  let j = ref 0 in
  try
    for i = Array.length a - 1 downto 0 do
      if p (Array.unsafe_get a i) then begin j := i; raise Exit end
    done;
    raise Not_found
  with Exit -> !j

let array_rfind p a = a.(array_rfindi p a)

let array_iter_rev f a = for i = Array.length a - 1 downto 0 do f (Array.unsafe_get a i) done

(** [shuffle a] shuffles an array, giving a uniform random distribution *)
 let shuffle a =
   for i = pred (Array.length a) downto 1 do
     let j = Random.int (succ i) in
     if i <> j (* faster to omit this test with arrays of about 100000 elements or more *) then (
       let tmp = Array.unsafe_get a i in
       Array.unsafe_set a i (Array.unsafe_get a j);
       Array.unsafe_set a j tmp
     )
   done

(** [partition l n] splits [l] into [n] chunks *)
let partition l n =
  if n < 2 then [| l |] else
  let a = Array.make n [] in
  ExtList.List.iteri (fun i x -> let i = i mod n in a.(i) <- x :: a.(i)) l;
  a

let file_lines_exn file =
  Control.with_open_in_txt file begin fun ch ->
    Std.input_lines ch >> List.of_enum
  end

(** read lines from file skipping empty lines and comments (lines starting with '#') *)
let config_lines_exn file =
  Control.with_open_in_txt file begin fun ch ->
    Std.input_lines ch >> Enum.map String.strip >>
    Enum.filter (fun s -> s <> "" && s.[0] <> '#') >> List.of_enum
  end

let file_lines_exn file = try file_lines_exn file with exn -> Exn.fail ~exn "file_lines %s" file
let config_lines_exn file = try config_lines_exn file with exn -> Exn.fail ~exn "config_lines %s" file

let file_lines file = try file_lines_exn file with _ -> []
let config_lines file = try config_lines_exn file with _ -> []

let hashtbl_find h f k =
  try Hashtbl.find h k with Not_found -> let v = f () in Hashtbl.replace h k v; v

(** array must be sorted *)
let binary_search arr cmp x =
  let rec loop a b =
    match b - a with
    | 0 -> false
    | 1 -> cmp arr.(a) x = 0
    | n ->
      let mid = a + n / 2 in
      let v = arr.(mid) in
      match cmp v x with
      | 0 -> true 
      | 1 -> loop a mid
      | _ (* -1 *) -> loop (mid+1) b
  in
  loop 0 (Array.length arr)

(* hm FIXME? *)
let chunk n l =
  assert (n > 0);
  let chunks = ref [] in
  let get_chunk e =
    let rec loop acc = function
      | 0 -> acc
      | n -> match Enum.get e with None -> acc | Some x -> loop (x::acc) (n-1)
    in
    chunks := loop [] n :: !chunks
  in
  let rec loop e =
    match Enum.peek e with
    | None -> List.rev !chunks
    | _ -> get_chunk e; loop e
  in
  loop (List.enum l) 

(** [chunk_e e n] splits [e] into chunks of [n] elements each (except the last which can be shorter) *)
let chunk_e n e =
  assert (n > 0);
  let fin () = raise Enum.No_more_elements in
  Enum.from (fun () ->
    let i = ref n in
    if Enum.is_empty e then fin () else
    Enum.from (fun () -> match !i with 
      | 0 -> fin ()
      | _ -> decr i; match Enum.get e with None -> fin () | Some x -> x))

(* FIXME *)

let bytes_string_f f = (* oh ugly *)
  let a = abs_float f in
  if a < 1024. then sprintf "%dB" (int_of_float f) else
  if a < 1024. *. 1024. then sprintf "%dKB" (int_of_float (f /. 1024.)) else
  if a < 1024. *. 1024. *. 1024. then sprintf "%.1fMB" (f /. 1024. /. 1024.) else
  if a < 1024. *. 1024. *. 1024. *. 1024. then sprintf "%.1fGB" (f /. 1024. /. 1024. /. 1024.) else
  sprintf "%.1fTB" (f /. 1024. /. 1024. /. 1024. /. 1024.)

let bytes_string = bytes_string_f $ float_of_int
let bytes_string_i64 = bytes_string_f $ Int64.to_float

let bytes_of_words x = Sys.word_size / 8 * x
let bytes_of_words_f x = float (Sys.word_size / 8) *. x

let caml_words = bytes_string $ bytes_of_words
let caml_words_f = bytes_string_f $ bytes_of_words_f

(* EMXIF *)

module App(Info : sig val version : string val name : string end) = struct

let run main =
  Printexc.record_backtrace true;
  Log.self #info "%s started. Version %s. PID %u" Info.name Info.version (Unix.getpid ());
  try
    main ();
    Log.self #info "%s finished." Info.name
  with
    exn -> Log.self #error ~exn ~backtrace:true "%s aborted" Info.name

end

class timer = 
let tm = Unix.gettimeofday  in
object

val mutable start = tm ()
method reset = start <- tm ()
method get = tm () -. start
method gets = sprintf "%.6f" & tm () -. start
method get_str = Time.duration_str & tm () -. start

end

let speed n t = float n /. (max t epsilon_float)

let perform ?name f x =
  let t = new timer in
  try
    Option.may (Log.self #info "Action %S started") name;
    let () = f x in
    Option.may (fun name -> Log.self #info "Action %S finished (elapsed %s)" name t#get_str) name;
    true
  with
    exn ->
      let name = Option.map_default (Printf.sprintf " %S") "" name in
      Log.self #error ~exn ~backtrace:true "Action%s aborted with uncaught exception (elapsed %s)" name t#get_str;
      false

let log ?name f x = let (_:bool) = perform ?name f x in ()

let log_thread ?name f x =
  Thread.create (fun () -> log ?name f x) ()

(** Copy all data from [input] to [output] *)
let io_copy input output =
  try
    let size = 16 * 1024 in
    let s = String.create size in
    while true do
      let n = IO.input input s 0 size in
      if n = 0 then raise IO.No_more_input;
      ignore & IO.really_output output s 0 n
    done
  with IO.No_more_input -> ()

let io_null = IO.create_out (fun _ -> ()) (fun _ _ len -> len) id id

let compare_by f a b = compare (f a) (f b)
let compare2 f g (a,b) (a',b') =
  match f a a' with
  | 0 -> g b b'
  | x -> x

let hexdump str =
  let buf = Buffer.create 80 and num = ref 0 in
  let rec loop chars =
    match List.take 16 chars with
    | [] -> Buffer.contents buf
    | l ->
          bprintf buf "%08x|  " !num;
          num := !num + 16;
          let rec bytes pos = function
            | [] -> 
                blanks pos
            | x :: l ->
                if pos = 8 then Buffer.add_char buf ' ';
                Printf.bprintf buf "%02x " (Char.code x);
                bytes (pos + 1) l
          and blanks pos =
            if pos < 16 then begin
              if pos = 8 then
                Buffer.add_string buf "    "
              else
                Buffer.add_string buf "   ";
              blanks (pos + 1)
            end
          in
          bytes 0 l;
          Buffer.add_string buf " |";
          List.iter (fun ch -> Buffer.add_char buf (if ch >= '\x20' && ch <= '\x7e' then ch else '.')) l;
          Buffer.add_char buf '|';
          Buffer.add_char buf '\n';
          loop (List.drop 16 chars)
  in
   loop (String.explode str)

open Gc

let gc_diff st1 st2 =
  let allocated st = st.minor_words +. st.major_words -. st.promoted_words in
  let a = allocated st2 -. allocated st1 in
  let minor = st2.minor_collections - st1.minor_collections in
  let major = st2.major_collections - st1.major_collections in
  let compact = st2.compactions - st1. compactions in
  let heap = st2.heap_words - st1.heap_words in
  Printf.sprintf "allocated %10s, heap %10s, collection %d %d %d" (caml_words_f a) (caml_words heap) compact major minor

let gc_show name f x =
  let t = new timer in
  let st = Gc.quick_stat () in
  Std.finally (fun () -> let st2 = Gc.quick_stat () in Log.main #info "GC DIFF %s : %s, elapsed %s" name (gc_diff st st2) t#get_str) f x

let gc_settings () =
  let gc = Gc.get () in
  sprintf "minor %s incr %s major %d%% compact %d%% policy %d" 
    (caml_words gc.Gc.minor_heap_size) 
    (caml_words gc.Gc.major_heap_increment)
    gc.Gc.space_overhead
    gc.Gc.max_overhead
    gc.Gc.allocation_policy

(*
let mem_usage v =
  let x = Objsize.objsize v in
  Printf.sprintf "%s (data %s)" (Action.bytes_string (Objsize.size_with_headers x)) (Action.bytes_string (Objsize.size_without_headers x))
*)

(** not closing underlying io *)
let count_bytes_to count out =
  IO.create_out
    ~write:(fun c -> count := Int64.succ !count; IO.write out c)
    ~output:(fun s o l -> count := Int64.add !count (Int64.of_int l); IO.output out s o l)
    ~flush:(fun () -> IO.flush out)
    ~close:(fun () -> !count)

let count_bytes out = let count = ref 0L in count_bytes_to count out

let bench count f =
  Gc.compact ();
  let t = new timer in
  let st = Gc.quick_stat () in
  let res = Exn.map (fun () -> for i = 1 to count do f () done) () in
  let st2 = Gc.quick_stat () in
  let elapsed = t#get in
  let res = match res with
  | `Ok () -> "ok"
  | `Exn exn -> "exn " ^ Exn.str exn
  in
  sprintf "%s, elapsed %s, %.2f/sec : %s" (gc_diff st st2) (Time.duration_str elapsed) (speed count elapsed) res

let run_bench count l =
  let max_len = List.fold_left (fun acc (name,_) -> max acc (String.length name)) 0 l in
  let align s = String.make (max 0 (max_len - String.length s)) ' ' ^ s in
  printfn "run_bench %d cases (count %d)" (List.length l) count;
  List.iter (fun (name,f) -> printfn "%s : %s" (align name) (bench count f)) l

(* sorting DynArray *)

let rec quick_sort d left right cmp =
  let i = ref left and j = ref right in
  let pivot = DynArray.unsafe_get d ((left + right) / 2) in
  while !i <= !j do
    while cmp (DynArray.unsafe_get d !i) pivot  = -1 do incr i done;
    while cmp pivot (DynArray.unsafe_get d !j)  = -1 do decr j done;
    if !i <= !j then begin
      let tmp = DynArray.unsafe_get d !i in
      DynArray.unsafe_set d !i (DynArray.unsafe_get d !j);
      DynArray.unsafe_set d !j tmp;
      incr i; decr j
    end;
  done;
  if left < !j then quick_sort d left !j cmp;
  if !i < right then quick_sort d !i right cmp

let quick_sort d cmp = quick_sort d 0 (DynArray.length d - 1) cmp

(**
  find the minimum element in the list
  @param cmp compare function, default [Pervasives.compare]
  @raise Empty_list when list is emtpty
*)
let list_min ?(cmp=compare) l =
  List.fold_left (fun x y -> if cmp x y < 0 then x else y) (List.hd l) l

(** command-line arguments *)
let args = List.tl (Array.to_list Sys.argv)

(** run [cb] after waiting for [delay].
  @param cb can return [false] to stop the thread, [true] otherwise
  @param now - show wether call cb before the first delay passes
  @return created thread
*)
let run_periodic ~delay ?(now=false) cb =
  let first_res = ref true in
  let th = ref None in
  let rec thread_fun f =
    Thread.delay delay;
    let res = try f () with exn -> Log.self #warn ~exn "uncaught exception : run periodic"; false in
    if res then
      thread_fun f
    else () (* exit *)
  in
  let th_ = Thread.create (fun () ->
    if now then first_res := cb ();
    if !first_res then thread_fun cb
  ) () in
  th := Some th_;
  th_
