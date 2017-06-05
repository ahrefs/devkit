(** Memory reporting - GC and OS, optionally malloc *)

open Prelude
open ExtLib
open Printf

let log = Log.from "memory"

type t = {
  rss : int; (** resident set size *)
  vsize : int; (** virtual memory size *)
  nr_maps : int; (** number of VM mappings *)
  swap_used : int; (** used swap size *)
}

let get_num = int_of_string $ String.replace_chars (fun c -> if Stre.ASCII.is_digit c then String.of_char c else "")

let pagesize = Int64.to_int ExtUnix.Specific.(sysconf PAGESIZE)

(** @return virtual memory info *)
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

let show_gc_heap ?(st=Gc.quick_stat ()) () =
  let open Action in
  sprintf "%s (max %s, chunks %d)"
      (caml_words st.Gc.heap_words)
      (caml_words st.Gc.top_heap_words)
      st.Gc.heap_chunks

let show_gc_info () =
  let open Action in
  let st = Gc.quick_stat () in
  let gc_heap = show_gc_heap ~st () in
  let gc_ctrs =
    sprintf "%s %s %s"
        (caml_words_f st.Gc.minor_words)
        (caml_words_f st.Gc.promoted_words)
        (caml_words_f st.Gc.major_words)
  in
  let gc_coll =
    sprintf "%u %u %u"
        st.Gc.compactions
        st.Gc.major_collections
        st.Gc.minor_collections
  in
  sprintf "GC: Heap: %s Counters(mi,pr,ma): %s Collections(mv,ma,mi): %s" gc_heap gc_ctrs gc_coll

let show_lwt_info () =
  let (r, w, t) = Lwt_engine.(readable_count (), writable_count (), timer_count ()) in
  sprintf "lwt readable %d, writable %d, timer %d" r w t

(* hooks for Memory_gperftools *)
let show_crt_info = ref (fun () -> "MALLOC: ?")
let malloc_release = ref (ignore : unit -> unit)

let reclaim_s () =
  let module A = Action in
  let st1 = Gc.stat () in
  let { rss; _ } = get_vm_info () in
  let t1 = Time.now () in
  Gc.compact ();
  let t2 = Time.now () in
  !malloc_release ();
  let t3 = Time.now () in
  let st3 = Gc.stat () in
  let { rss=rss'; _ } = get_vm_info () in
  let changed f a b =
    if a = b then sprintf "= %s" (f a) else sprintf "%s -> %s" (f a) (f b)
  in
  sprintf "Memory.reclaim: heap %s live %s freelist %s (%s), rss %s"
    (changed A.caml_words st1.heap_words st3.heap_words)
    (changed A.caml_words st1.live_words st3.live_words)
    (changed string_of_int st1.free_blocks st3.free_blocks)
    (Time.duration_str @@ t2 -. t1)
    (if !malloc_release == ignore then A.bytes_string rss
     else sprintf "%s (%s)" (changed A.bytes_string rss rss') (Time.duration_str @@ t3 -. t2))

let reclaim () = log #info "%s" @@ reclaim_s ()

let reclaim_silent () =
  Gc.compact ();
  !malloc_release ()

let (add_stats,new_stats,log_stats,get_stats) =
  let f_print = ref [] in (* called in reverse - and it is fine *)
  let f_get = ref [] in
  let log_stats () =
    List.iter (fun f -> f ()) !f_print;
    List.iter (fun f -> log #info_s @@ f ()) !f_get
  in
  let get_stats () = List.map (fun f -> f ()) !f_get in
  (tuck f_print), (tuck f_get), log_stats, get_stats

let track_global = ref []
let show_global_reachable () =
  let l = List.map (fun (name,repr) -> sprintf "%s %s" name (Action.caml_words @@ Obj.reachable_words repr)) !track_global in
  sprintf "reachable: %s" (String.concat " " l)
let track_global name var = tuck track_global (name,Obj.repr var)

let show_c_info () = sprintf "%s. %s" (show_vm_info ()) (!show_crt_info ())

let show_all_info () =
  [
    show_c_info ();
    show_gc_info ();
    show_lwt_info ();
  ]

let log_all_info () = show_all_info () |> List.iter log#info_s

let () = new_stats show_c_info
let () = new_stats show_gc_info
let () = new_stats show_lwt_info
let () = new_stats show_global_reachable
