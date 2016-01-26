(** File system *)

val enum_dir : Unix.dir_handle -> string Enum.t
val with_readdir : string -> (Unix.dir_handle -> 'a) -> 'a

(** [f fd path rel] gets invoked for each file under [dirname] where
[fd] is a read-only [Unix.file_descr], [path] is full path and [rel] - path relative to [dirname] *)
val iter_names : string -> (Unix.file_descr -> string -> string -> unit) -> unit

(** [iter_names_q dirname (fun [path] [rel] -> ...)] *)
val iter_names_q : string -> (string -> string -> unit) -> unit

(** [iter_files dirname (fun [path] [ic] -> ...)] *)
val iter_files : string -> (string -> in_channel -> unit) -> unit

val open_out_append_bin : string -> out_channel
val open_out_append_text : string -> out_channel

val save_as : string -> ?mode:Unix.file_perm -> (out_channel -> 'a) -> unit
