(** Memory reporting - GC and OS, optionally malloc *)

open Prelude
open ExtLib
open Printf

let log = Log.from "memory"

type t = { rss : int; vsize : int; nr_maps : int; swap_used : int; }

let get_num = int_of_string $ String.replace_chars (fun c -> if Stre.ASCII.is_digit c then String.of_char c else "")

let pagesize = Int64.to_int ExtUnix.Specific.(sysconf PAGESIZE)

let get_vm_info () =
  let (vsize,rss) =
    match Action.file_lines "/proc/self/statm" with
    | [] -> Log.self #warn "cannot read /proc/self/statm, no VM info"; (0,0)
    | s::_ -> Scanf.sscanf s "%d %d" (fun vsize rss -> (pagesize * vsize, pagesize * rss))
  in
  let nr_maps = List.length @@ Action.file_lines ("/proc/self/maps") in (* FIXME deleted *)
  (* process smaps *)
  let swap_used =
    Action.file_lines ("/proc/self/smaps") |>
    List.fold_left (fun acc s -> if String.starts_with s "Swap:" then acc + get_num s else acc) 0
  in
  { rss; vsize; nr_maps; swap_used = swap_used * 1024; }

let show_vm_info () =
  let bytes = Action.bytes_string in
  let { rss; vsize; nr_maps; swap_used } = get_vm_info () in
  sprintf "VM: rss %s, vsz %s, swap %s, maps %d" (bytes rss) (bytes vsize) (bytes swap_used) nr_maps

let show_gc_info () =
  sprintf "GC: %s" (Varz.gc_info ())

(* hooks for Memory_gperftools *)
let show_crt_info = ref (fun () -> "MALLOC: ?")
let malloc_release = ref (ignore : unit -> unit)

let show_all_info () = sprintf "%s\n%s. %s" (show_gc_info ()) (show_vm_info ()) (!show_crt_info ())

let log_all_info () = log #info "%s" @@ show_all_info ()

let reclaim_s () =
  let open Gc in
  let module A = Action in
  let heap_words = (stat ()).heap_words in
  let { rss; _ } = get_vm_info () in
  let t1 = Time.now () in
  Gc.compact ();
  let t2 = Time.now () in
  !malloc_release ();
  let t3 = Time.now () in
  let heap_words' = (stat ()).heap_words in
  let { rss=rss'; _ } = get_vm_info () in
  let changed f a b =
    if a = b then sprintf "= %s" (f a) else sprintf "%s -> %s" (f a) (f b)
  in
  sprintf "Memory.reclaim: heap %s (%s), rss %s"
    (changed A.caml_words heap_words heap_words')
    (Time.duration_str @@ t2 -. t1)
    (if !malloc_release == ignore then A.bytes_string rss
     else sprintf "%s (%s)" (changed A.bytes_string rss rss') (Time.duration_str @@ t3 -. t2))

let reclaim () = log #info "%s" @@ reclaim_s ()
