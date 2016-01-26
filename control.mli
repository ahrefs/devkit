(** Control flow *)

(** [bracket resource destroy k]
    @return [k resource] and guarantee that [resource] is [destroy]'ed at the end. *)
val bracket : 'a -> ('a -> unit) -> ('a -> 'b) -> 'b

(** [wrapped acc result k]

  Computation [k] accumulates result into resource [acc] which
  is guaranteed to be released at the end. Rarely useful (e.g. {!IO.output_string})
  @return [result acc] *)
val wrapped : 'a -> ('a -> 'b) -> ('a -> unit) -> 'b


(** File IO *)

(** Protected file IO, stdlib interface *)

val with_open_in_bin : string -> (in_channel -> 'a) -> 'a
val with_open_in_txt : string -> (in_channel -> 'a) -> 'a

val with_open_out_bin : string -> (out_channel -> 'a) -> 'a
val with_open_out_txt : string -> (out_channel -> 'a) -> 'a

val with_open_out_temp_file : ?temp_dir:string -> mode:open_flag list -> (string * out_channel -> 'a) -> 'a
val with_open_out_temp_bin : (string * out_channel -> 'a) -> 'a
val with_open_out_temp_txt : (string * out_channel -> 'a) -> 'a

(** Protected file IO, extlib interface *)

val wrapped_output : 'a IO.output -> ('a IO.output -> unit) -> 'a
val wrapped_outs : (string IO.output -> unit) -> string

val with_input : IO.input -> (IO.input -> 'a) -> 'a
val with_input_bin : string -> (IO.input -> 'a) -> 'a
val with_input_txt : string -> (IO.input -> 'a) -> 'a

val with_output : unit IO.output -> (unit IO.output -> 'a) -> 'a
val with_output_bin : string -> (unit IO.output -> 'a) -> 'a
val with_output_txt : string -> (unit IO.output -> 'a) -> 'a


(** Misc. *)

val locked : Mutex.t -> (unit -> 'a) -> 'a
val with_opendir : string -> (Unix.dir_handle -> 'b) -> 'b
