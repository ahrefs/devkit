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

val with_opendir : string -> (Unix.dir_handle -> 'b) -> 'b


module Rate_limit : sig
  type t
  val none : t
  val create : ?burst_capacity:int -> allowed_per_sec:float -> unit -> t
  (** Create a token-bucket limiter with the given sustained rate and capacity
      for ten seconds of traffic (at least one token). The bucket starts full.
      @param burst_capacity limits the size of a burst when token bucket is full
      @param allowed_per_sec number of tokens refilled per second, ie asymptotic
        max throughtput
      @raise Invalid_argument if [allowed_per_sec] is not finite and positive. *)

  val take_rate_limited_count: t -> int
  (** How many attempts have been rate limited since last time this was called? *)

  val attempt : t -> bool
  (** Attempt to perform one action. Return [true] if allowed by rate limiter. *)
end
