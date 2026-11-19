(** gzip IO *)

(** [input io] decompresses the gzip data read from [io] (a single gzip member) *)
val input : IO.input -> IO.input

(** [output io] compresses what is written to it as gzip and writes that to [io] *)
val output : 'a IO.output -> 'a IO.output

val input_ch : Stdlib.in_channel -> IO.input
val output_ch : Stdlib.out_channel -> unit IO.output

(** [string s] is [s] compressed as gzip *)
val string : string -> string

(** [to_string s] decompresses gzip data [s] *)
val to_string : string -> string
