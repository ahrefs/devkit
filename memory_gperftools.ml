(** Memory reporting for gperftools, call [setup] in every binary linked with gperftools *)

let show_crt_info () =
  let bytes = Action.bytes_string in
  let p x = try bytes @@ Gperftools.get_numeric_property x with _ -> "?" in
  Printf.sprintf "MALLOC: size %s, used %s, free %s"
    (p "generic.heap_size") (p "generic.current_allocated_bytes") (p "tcmalloc.pageheap_free_bytes")

let setup () =
  Gperftools.set_memory_release_rate 10.;
  Memory.show_crt_info := show_crt_info;
  Memory.malloc_release := Gperftools.release_free_memory;
  ()
