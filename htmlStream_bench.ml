(** HtmlStream GC Benchmark Suite

    This suite is designed to stress the OCaml garbage collector with various
    allocation patterns and heap shape changes over time. Each benchmark targets
    different GC behaviors:
    - Minor collection pressure (ephemeral allocations)
    - Major collection pressure (long-lived data)
    - Heap fragmentation
    - Generational hypothesis violations
    - Large object handling
*)

open Devkit

(* Benchmark configuration *)
type config = {
  warmup_iterations : int;
  benchmark_iterations : int;
  verbose : bool;
  gc_stats : bool;
}

let default_config = {
  warmup_iterations = 3;
  benchmark_iterations = 10;
  verbose = false;
  gc_stats = true;
}

(* GC statistics collection *)
type gc_snapshot = {
  minor_words : float;
  promoted_words : float;
  major_words : float;
  minor_collections : int;
  major_collections : int;
  heap_words : int;
  heap_chunks : int;
  live_words : int;
  free_words : int;
  largest_free : int;
  fragments : int;
  compactions : int;
  top_heap_words : int;
}

let snapshot_gc () =
  let stat = Gc.stat () in
  {
    minor_words = stat.Gc.minor_words;
    promoted_words = stat.Gc.promoted_words;
    major_words = stat.Gc.major_words;
    minor_collections = stat.Gc.minor_collections;
    major_collections = stat.Gc.major_collections;
    heap_words = stat.Gc.heap_words;
    heap_chunks = stat.Gc.heap_chunks;
    live_words = stat.Gc.live_words;
    free_words = stat.Gc.free_words;
    largest_free = stat.Gc.largest_free;
    fragments = stat.Gc.fragments;
    compactions = stat.Gc.compactions;
    top_heap_words = stat.Gc.top_heap_words;
  }

let diff_gc before after = {
  minor_words = after.minor_words -. before.minor_words;
  promoted_words = after.promoted_words -. before.promoted_words;
  major_words = after.major_words -. before.major_words;
  minor_collections = after.minor_collections - before.minor_collections;
  major_collections = after.major_collections - before.major_collections;
  heap_words = after.heap_words;
  heap_chunks = after.heap_chunks;
  live_words = after.live_words;
  free_words = after.free_words;
  largest_free = after.largest_free;
  fragments = after.fragments;
  compactions = after.compactions - before.compactions;
  top_heap_words = max after.top_heap_words before.top_heap_words;
}

let print_gc_stats name stats =
  Printf.printf "\n=== GC Stats for %s ===\n" name;
  Printf.printf "Minor words allocated: %.0f\n" stats.minor_words;
  Printf.printf "Promoted words: %.0f\n" stats.promoted_words;
  Printf.printf "Major words allocated: %.0f\n" stats.major_words;
  Printf.printf "Minor collections: %d\n" stats.minor_collections;
  Printf.printf "Major collections: %d\n" stats.major_collections;
  Printf.printf "Heap words: %d\n" stats.heap_words;
  Printf.printf "Live words: %d\n" stats.live_words;
  Printf.printf "Fragments: %d\n" stats.fragments;
  Printf.printf "Compactions: %d\n" stats.compactions

(* Benchmark runner *)
let run_benchmark ~config ~name ~generate_html ~process_elem () =
  Printf.printf "Running benchmark: %s\n" name;

  (* Warmup *)
  for _ = 1 to config.warmup_iterations do
    let html = generate_html () in
    let ctx = HtmlStream.init () in
    HtmlStream.parse ~ctx process_elem html
  done;

  (* Force major GC to start from clean state *)
  Gc.full_major ();

  let before = snapshot_gc () in
  let start = Sys.time () in

  (* Actual benchmark *)
  for _ = 1 to config.benchmark_iterations do
    let html = generate_html () in
    let ctx = HtmlStream.init () in
    HtmlStream.parse ~ctx process_elem html
  done;

  let elapsed = Sys.time () -. start in
  let after = snapshot_gc () in
  let stats = diff_gc before after in

  Printf.printf "  Time: %.3f seconds\n" elapsed;
  Printf.printf "  Throughput: %.0f iterations/sec\n"
    (float_of_int config.benchmark_iterations /. elapsed);

  if config.gc_stats then print_gc_stats name stats;

  (elapsed, stats)

(* ============================================================================
   Benchmark 1: Small String Pressure (Minor GC stress)
   Creates many small text nodes, causing frequent minor collections
   ============================================================================ *)
let bench_small_strings config =
  let generate_html () =
    let buf = Buffer.create (1024 * 1024) in
    for i = 1 to 10000 do
      Buffer.add_string buf "<p>";
      Buffer.add_string buf (string_of_int i);
      Buffer.add_string buf " small text ";
      Buffer.add_string buf (String.make 10 (char_of_int (65 + i mod 26)));
      Buffer.add_string buf "</p>";
    done;
    Buffer.contents buf
  in

  let collected_texts = ref [] in
  let process_elem = function
    | HtmlStream.Text t ->
        (* Simulate some retention to prevent immediate collection *)
        if Random.int 100 < 10 then
          collected_texts := HtmlStream.Raw.project t :: !collected_texts
    | _ -> ()
  in

  run_benchmark ~config ~name:"Small String Pressure" ~generate_html ~process_elem ()

(* ============================================================================
   Benchmark 2: Attribute List Pressure
   Creates tags with many attributes, stressing list allocation and reversal
   ============================================================================ *)
let bench_attribute_lists config =
  let generate_html () =
    let buf = Buffer.create (1024 * 1024 * 2) in
    for i = 1 to 5000 do
      Buffer.add_string buf "<div";
      let num_attrs = 20 + (i mod 30) in  (* Varying attribute counts *)
      for j = 1 to num_attrs do
        Printf.bprintf buf " attr%d=\"value%d\"" j (i * j);
      done;
      Buffer.add_string buf ">";
      Buffer.add_string buf (string_of_int i);
      Buffer.add_string buf "</div>";
    done;
    Buffer.contents buf
  in

  let total_attrs = ref 0 in
  let process_elem = function
    | HtmlStream.Tag (_, attrs) ->
        total_attrs := !total_attrs + List.length attrs
    | _ -> ()
  in

  run_benchmark ~config ~name:"Attribute List Pressure" ~generate_html ~process_elem ()

(* ============================================================================
   Benchmark 3: Large Block Allocations
   Creates large script/style blocks for major heap pressure
   ============================================================================ *)
let bench_large_blocks config =
  let generate_html () =
    let buf = Buffer.create (1024 * 1024 * 5) in
    for i = 1 to 1000 do
      let size = 1024 * (1 + i mod 100) in  (* Varying sizes 1KB-100KB *)
      Buffer.add_string buf "<script>";
      Buffer.add_string buf (String.make size 'x');
      Buffer.add_string buf "</script>";

      if i mod 2 = 0 then begin
        Buffer.add_string buf "<style>";
        Buffer.add_string buf (String.make (size / 2) 'y');
        Buffer.add_string buf "</style>";
      end;
    done;
    Buffer.contents buf
  in

  let retained_blocks = ref [] in
  let process_elem = function
    | HtmlStream.Script (_, s) | HtmlStream.Style (_, s) ->
        (* Retain some blocks to create old generation pressure *)
        if Random.int 100 < 20 then
          retained_blocks := s :: !retained_blocks
    | _ -> ()
  in

  run_benchmark ~config ~name:"Large Block Allocations" ~generate_html ~process_elem ()

(* ============================================================================
   Benchmark 4: Heap Shape Morphing
   Alternates between different allocation patterns to change heap shape
   ============================================================================ *)
let bench_morphing_heap config =
  let generate_html () =
    let buf = Buffer.create (1024 * 1024 * 3) in
    for phase = 1 to 100 do
      (* Phase 1: Many small text nodes *)
      if phase mod 3 = 0 then begin
        for _ = 1 to 100 do
          Buffer.add_string buf "<span>small</span>";
        done
      end;

      (* Phase 2: Deep nesting with attributes *)
      if phase mod 3 = 1 then begin
        for depth = 1 to 20 do
          Printf.bprintf buf "<div class=\"level%d\" id=\"node%d\">" depth (phase * depth);
        done;
        Buffer.add_string buf "nested content";
        for _ = 1 to 20 do
          Buffer.add_string buf "</div>";
        done
      end;

      (* Phase 3: Large blocks *)
      if phase mod 3 = 2 then begin
        Buffer.add_string buf "<script>";
        Buffer.add_string buf (String.make (10240 * phase) 'z');
        Buffer.add_string buf "</script>";
      end
    done;
    Buffer.contents buf
  in

  let phase_data = ref [] in
  let process_elem elem =
    (* Randomly retain elements from different phases *)
    if Random.int 100 < 15 then
      phase_data := elem :: !phase_data;
    (* Periodically clear old data to simulate phase changes *)
    if List.length !phase_data > 1000 then
      phase_data := List.tl !phase_data
  in

  run_benchmark ~config ~name:"Morphing Heap Shape" ~generate_html ~process_elem ()

(* ============================================================================
   Benchmark 5: Fragmentation Stress
   Creates and releases objects of varying sizes to fragment the heap
   ============================================================================ *)
let bench_fragmentation config =
  let generate_html () =
    let buf = Buffer.create (1024 * 1024 * 4) in
    let sizes = [| 10; 100; 1000; 10000; 100; 10; 5000; 50; 500 |] in
    for i = 1 to 2000 do
      let size = sizes.(i mod Array.length sizes) in

      (* Mix different element types with varying sizes *)
      match i mod 4 with
      | 0 -> (* Text of varying length *)
          Printf.bprintf buf "<p>%s</p>" (String.make size 'a')
      | 1 -> (* Tags with varying attributes *)
          Buffer.add_string buf "<div";
          for j = 1 to (size / 100 + 1) do
            Printf.bprintf buf " a%d=\"%s\"" j (String.make (size / 20) 'b');
          done;
          Buffer.add_string buf ">content</div>"
      | 2 -> (* Script blocks *)
          Printf.bprintf buf "<script>%s</script>" (String.make size 'c')
      | _ -> (* Mixed content *)
          for _ = 1 to (size / 100) do
            Buffer.add_string buf "<span>x</span>";
          done
    done;
    Buffer.contents buf
  in

  (* Use a hashtable to create non-uniform retention *)
  let retained = Hashtbl.create 1000 in
  let counter = ref 0 in
  let process_elem elem =
    incr counter;
    (* Retain elements with prime-number intervals to create gaps *)
    if !counter mod 7 = 0 || !counter mod 11 = 0 || !counter mod 13 = 0 then
      Hashtbl.replace retained !counter elem;
    (* Occasionally clear old entries *)
    if !counter mod 100 = 0 then
      Hashtbl.iter (fun k _ ->
        if k < !counter - 500 then Hashtbl.remove retained k
      ) retained
  in

  run_benchmark ~config ~name:"Heap Fragmentation" ~generate_html ~process_elem ()

(* ============================================================================
   Benchmark 6: Generational Hypothesis Violation
   Creates objects that violate the generational hypothesis by updating old objects
   ============================================================================ *)
let bench_generational_violation config =
  let generate_html () =
    let buf = Buffer.create (1024 * 1024 * 2) in
    for batch = 1 to 100 do
      (* Create batches of elements *)
      for i = 1 to 100 do
        Printf.bprintf buf "<div id=\"gen_%d_%d\">" batch i;
        Printf.bprintf buf "Generation %d Item %d" batch i;
        for j = 1 to batch do
          Printf.bprintf buf "<span class=\"ref_%d\">%d</span>" j (batch * i * j);
        done;
        Buffer.add_string buf "</div>";
      done;
    done;
    Buffer.contents buf
  in

  (* Multiple generations of retained data *)
  let old_generation = ref [] in
  let middle_generation = ref [] in
  let young_generation = ref [] in
  let counter = ref 0 in

  let process_elem elem =
    incr counter;
    (* Rotate through generations *)
    if !counter mod 100 = 0 then begin
      old_generation := !middle_generation;
      middle_generation := !young_generation;
      young_generation := [];
    end;
    (* Add to young generation *)
    young_generation := elem :: !young_generation;

    (* Occasionally reference old objects from new ones *)
    if !counter mod 50 = 0 then begin
      let mixed = !old_generation @ !young_generation in
      young_generation :=
        let rec take n = function
          | [] -> []
          | _ when n <= 0 -> []
          | h :: t -> h :: take (n-1) t
        in
        List.rev (take 10 (List.rev mixed))
    end
  in

  run_benchmark ~config ~name:"Generational Violation" ~generate_html ~process_elem ()

(* ============================================================================
   Benchmark 7: Allocation Rate Variation
   Varies allocation rate over time to test GC adaptation
   ============================================================================ *)
let bench_variable_rate config =
  let generate_html () =
    let buf = Buffer.create (1024 * 1024 * 3) in
    for phase = 1 to 50 do
      let intensity =
        (* Create sine wave pattern of allocation intensity *)
        int_of_float (50.0 +. 45.0 *. sin (float_of_int phase *. 0.3))
      in

      (* Low intensity phase *)
      if intensity < 30 then begin
        for _ = 1 to intensity do
          Buffer.add_string buf "<p>low intensity</p>";
        done
      end
      (* Medium intensity phase *)
      else if intensity < 70 then begin
        for i = 1 to intensity * 10 do
          Printf.bprintf buf "<div id=\"med%d\">content %d</div>" i i;
        done
      end
      (* High intensity phase *)
      else begin
        for i = 1 to intensity * 20 do
          Buffer.add_string buf "<span";
          for j = 1 to 5 do
            Printf.bprintf buf " a%d=\"v%d\"" j (i*j);
          done;
          Printf.bprintf buf ">%d</span>" i;
        done
      end
    done;
    Buffer.contents buf
  in

  let allocation_history = Array.make 1000 [] in
  let index = ref 0 in

  let process_elem elem =
    (* Circular buffer to maintain history *)
    let i = !index mod Array.length allocation_history in
    allocation_history.(i) <- elem :: allocation_history.(i);
    incr index;
    (* Periodically clear old parts of history *)
    if !index mod 100 = 0 then begin
      let clear_index = (!index - 500) mod Array.length allocation_history in
      if clear_index >= 0 then
        allocation_history.(clear_index) <- []
    end
  in

  run_benchmark ~config ~name:"Variable Allocation Rate" ~generate_html ~process_elem ()

(* ============================================================================
   Benchmark 8: Reference Graph Complexity
   Creates complex reference patterns to stress GC marking phase
   ============================================================================ *)
let bench_complex_references config =
  let generate_html () =
    let buf = Buffer.create (1024 * 1024 * 2) in
    (* Create interconnected structure *)
    for layer = 1 to 20 do
      for node = 1 to 50 do
        Printf.bprintf buf "<div class=\"layer_%d node_%d\">" layer node;
        (* Cross-references to other layers *)
        for ref_layer = 1 to 5 do
          for ref_node = 1 to 10 do
            Printf.bprintf buf "<a href=\"#layer_%d_node_%d\">ref</a>"
              ((layer + ref_layer) mod 20 + 1)
              ((node + ref_node) mod 50 + 1);
          done;
        done;
        Buffer.add_string buf "</div>";
      done;
    done;
    Buffer.contents buf
  in

  (* Graph-like structure to retain elements *)
  let graph = Hashtbl.create 1000 in
  let edges = ref [] in
  let node_counter = ref 0 in

  let process_elem elem =
    incr node_counter;
    let node_id = !node_counter in
    Hashtbl.add graph node_id elem;

    (* Create edges to previous nodes *)
    if node_id > 10 then begin
      for _ = 1 to Random.int 5 + 1 do
        let target = Random.int (node_id - 1) + 1 in
        edges := (node_id, target) :: !edges;
      done
    end;

    (* Occasionally prune old parts of graph *)
    if node_id mod 200 = 0 then begin
      edges := List.filter (fun (s, t) -> s > node_id - 1000 && t > node_id - 1000) !edges;
      Hashtbl.iter (fun k _ ->
        if k < node_id - 1000 then Hashtbl.remove graph k
      ) graph
    end
  in

  run_benchmark ~config ~name:"Complex Reference Graph" ~generate_html ~process_elem ()

(* Main benchmark suite runner *)
let run_all_benchmarks ?(config = default_config) () =
  Printf.printf "\n========================================\n";
  Printf.printf "  HtmlStream GC Benchmark Suite\n";
  Printf.printf "========================================\n\n";

  Printf.printf "Configuration:\n";
  Printf.printf "  Warmup iterations: %d\n" config.warmup_iterations;
  Printf.printf "  Benchmark iterations: %d\n" config.benchmark_iterations;
  Printf.printf "  GC stats: %s\n" (if config.gc_stats then "enabled" else "disabled");
  Printf.printf "\n";

  (* Set GC parameters for more predictable behavior *)
  let original_gc = Gc.get () in
  Gc.set { original_gc with
    Gc.minor_heap_size = 262144;  (* 256KB minor heap *)
    Gc.major_heap_increment = 126976;  (* ~124KB increment *)
  };

  let results = [
    ("Small Strings", bench_small_strings config);
    ("Attribute Lists", bench_attribute_lists config);
    ("Large Blocks", bench_large_blocks config);
    ("Morphing Heap", bench_morphing_heap config);
    ("Fragmentation", bench_fragmentation config);
    ("Generational", bench_generational_violation config);
    ("Variable Rate", bench_variable_rate config);
    ("Complex Refs", bench_complex_references config);
  ] in

  Printf.printf "\n\n========================================\n";
  Printf.printf "  Summary\n";
  Printf.printf "========================================\n\n";

  let total_time = List.fold_left (fun acc (_, (time, _)) -> acc +. time) 0.0 results in

  List.iter (fun (name, (time, stats)) ->
    Printf.printf "%-20s: %7.3fs | Minor GCs: %4d | Major GCs: %3d | Allocated: %.0fMB\n"
      name time stats.minor_collections stats.major_collections
      (stats.minor_words /. 1_000_000.0)
  ) results;

  Printf.printf "\nTotal time: %.3f seconds\n" total_time;

  (* Restore original GC settings *)
  Gc.set original_gc

(* Command-line interface *)
let () =
  let config = ref default_config in

  let usage = "Usage: " ^ Sys.argv.(0) ^ " [options]" in
  let spec = [
    ("-warmup", Arg.Int (fun n -> config := { !config with warmup_iterations = n }),
     "Number of warmup iterations");
    ("-iterations", Arg.Int (fun n -> config := { !config with benchmark_iterations = n }),
     "Number of benchmark iterations");
    ("-verbose", Arg.Unit (fun () -> config := { !config with verbose = true }),
     "Enable verbose output");
    ("-no-gc-stats", Arg.Unit (fun () -> config := { !config with gc_stats = false }),
     "Disable GC statistics output");
  ] in

  Arg.parse spec (fun _ -> ()) usage;
  run_all_benchmarks ~config:!config ()