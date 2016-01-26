
(** [period p f]

    @return a function [pf] such that [pf ()] = [f i] when [i]
    mod [p] = 0, and [()] otherwise. *)
val period : int -> (int -> unit) -> (unit -> unit)

(** [timely p f]

    @return a function [pf] such that [pf ()] = [f ()] if the
    last execution of [pf ()] was done more than [p] seconds ago, or
    [()] otherwise. *)
val timely : float -> (unit -> unit) -> (unit -> unit)

(** Combination of the above, see the code for more info. *)
val timely_counter : float -> (int -> unit) -> (unit -> unit)

(** run [f] in thread periodically once in [delay] seconds.
  @param f returns [false] to stop the thread, [true] otherwise
  @param now default [false]
*)
val thread_run_periodic : delay:float -> ?now:bool -> (unit -> bool) -> unit


(** Enum utilities *)

(** [uniq f e]

    @return [enum] that will not contain two values [x] and [x'] such
    that [f x] = [f x'].
*)
val uniq : ('a -> 'b) -> 'a Enum.t -> 'a Enum.t

(** [all_uniq f e]

    @return [true] iff there is no two values [x] and [x'] in [e] such
    that [f x] = [f x'].
*)
val all_uniq : ('a -> 'b) -> 'a Enum.t -> bool

(** [chunk_e n e] splits enum [e] into chunks of [n] elements each (except the last which can be shorter).
  NB the order in result is not specified *)
val chunk_e : int -> 'a Enum.t -> 'a Enum.t Enum.t


(** List utilities *)

(**
  find the minimum element in the list
  @param cmp compare function, default [Pervasives.compare]
  @raise Empty_list when list is empty
*)
val list_min : ?cmp:('a -> 'a -> int) -> 'a list -> 'a

(** [list_uniq f l]

    @return copy of [l] that will not contain two values [x] and [x'] such
    that [f x] = [f x'].
*)
val list_uniq : ('a -> 'b) -> 'a list -> 'a list

(** [list_sorted_uniq eq_f l]

    @return [l] without consecutive elements [x], [x'] such that [eq_f
    x] = [eq_f x'].
*)
val list_sorted_uniq : ('a -> 'a -> bool) -> 'a list -> 'a list

(** Get a random element from a list. *)
val list_random_exn : 'a list -> 'a
val list_random : 'a list -> 'a option

(** extract sublist from a list, e.g. [slice 1 3 \[0;1;2;3;4\]] will return [\[1;2;3\]]. *)
val slice : int -> int -> 'a list -> 'a list

(** Partitioning a list into chunks *)

(** [chunk_e n e] splits enum [e] into chunks of [n] elements each (except the last which can be shorter).
  NB the order in result is not specified FIXME? *)
val chunk : int -> 'a list -> 'a list list

(** [partition n l] splits [l] into [n] chunks, does not preserve the order of the elements. *)
val partition : int -> 'a list -> 'a list array
val unpartition : 'a list array -> 'a list

(** [stable_partition l n] splits [l] into [n] chunks, preserves the order of the elements. *)
val stable_partition : int -> 'a list -> 'a list list
val stable_unpartition : 'a list list -> 'a list

(** Printing *)

val catmap : ?sep:string -> ('a -> string) -> 'a list -> string
val strl : ('a -> string) -> 'a list -> string


(** Array utilities *)

val array_random_exn : 'a array -> 'a
val array_random : 'a array -> 'a option

(** [array_rfindi p a]

    @return index of first element matching [p] when iterating [a] in reverse.
    @raise Not_found if no such element exists.
*)
val array_rfindi : ('a -> bool) -> 'a array -> int

(** [array_rfind p a]

    @return value index of first element matching [p] when iterating [a] in reverse.
    @raise Not_found if no such element exists.
*)
val array_rfind : ('a -> bool) -> 'a array -> 'a

(** [array_iter_rev f a] calls [f] on each elements of [a] in reverse
    order. *)
val array_iter_rev : ('a -> 'b) -> 'a array -> unit

(** [shuffle a] shuffles an array, giving a uniform random distribution. *)
val shuffle : 'a array -> unit

(** array must be sorted *)
val binary_search' : 'a array -> ('a -> 'b -> int) -> 'b -> 'a option
val binary_search : 'a array -> ('a -> 'b -> int) -> 'b -> bool

(** [chunk_a n a] splits array [a] into chunks of [n] elements each (except the last which can be shorter), preserving
  the order of elements, i.e. reverse operation is [Array.concat] *)
val chunk_a : int -> 'a array -> 'a array list

(** Printing *)

val stra : ('a -> string) -> 'a array -> string


(** DynArray utilities *)

val quick_sort : 'a DynArray.t -> ?start:int -> ?n:int -> ('a -> 'a -> int) -> unit


(** Hashtbl utilities *)

(** [hashtbl_find ht f_default k] associates [f_default ()] to [k] in
    [ht], if no previous association exists.

    @return [Hashtbl.find ht k] if [k] is associated with an element
    in [ht], or [f_default ()] otherwise.
*)
val hashtbl_find : ('a, 'b) Hashtbl.t -> (unit -> 'b) -> 'a -> 'b


(** Gc / Memory utilities *)

(** Memory format parsing/pretty-printing *)

(** Parse memory size specification, accepts: MB KB 1MB 20gb *)
val parse_bytes_unit : string -> int

(** Pretty-print memory size in a way that can be parsed back by [parse_bytes_unit] *)
val show_bytes_unit : int -> string

(** Pretty-printing *)

val bytes_of_words : int -> int
val bytes_of_words_f : float -> float

(** short human-readable display for memory measures *)
val bytes_string : int -> string
val bytes_string_i64 : int64 -> string
val bytes_string_f : float -> string

val caml_words : int -> string
val caml_words_f : float -> string

(** string describing gc current settings. *)
val gc_diff : Gc.stat -> Gc.stat -> string

val gc_show : string -> ('a -> 'b) -> 'a -> 'b
val gc_settings : unit -> string


(** File IO *)

(** Counting bytes. Not closing underlying io. *)
val count_bytes_to : int64 ref -> 'a IO.output -> int64 IO.output
val count_bytes : 'a IO.output -> int64 IO.output

(** Copy all data from [input] to [output] *)
val io_copy : IO.input -> 'a IO.output -> unit

(** /dev/null -like *)
val io_null : unit IO.output

(** Extracting lines from a file. *)

val file_lines_exn : string -> string list
val file_lines : string -> string list

(** read lines from file skipping empty lines and comments (lines starting with '#') *)
val make_config_lines : string list -> string list
val config_lines_exn : string -> string list
val config_lines : string -> string list


(** Time utilities *)

class timer :
  object
    method get : float
    method get_str : string
    method reset : unit
  end

(** Timer running from the start of the program execution. *)
val uptime : timer
val speed : int -> float -> float


(** Log or time execution of a function *)

val log : ?name:string -> ('a -> unit) -> 'a -> unit
val log_do : ?name:string -> (unit -> unit) -> unit

val log_thread : ?name:string -> ('a -> unit) -> 'a -> Thread.t
val log_thread_do : ?name:string -> (unit -> unit) -> Thread.t
val perform : ?name:string -> ('a -> unit) -> 'a -> bool


(** Comparison *)

val compare_by : ('a -> 'b) -> 'a -> 'a -> int
val compare2 : ('a -> 'b -> int) -> ('c -> 'd -> int) -> 'a * 'c -> 'b * 'd -> int
val compare2_by : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'a * 'c -> int
val compare_fst : ('a -> 'b -> 'c) -> 'a * 'd -> 'b * 'e -> 'c


(** Benchmarking functions *)

val bench : ?compact:(unit -> unit) -> int -> (unit -> 'a) -> string
val run_bench : ?compact:(unit -> unit) -> int -> (string * (unit -> 'a)) list -> unit


(** Command-line arguments *)

(** Does not contains Sys.argv.(0). *)
val args : string list


(** Misc. *)

(** name01 name02 name09 name10 name11 -> name0{1..2} name{09..11} *)
val shell_sequence : string list -> string list

val hexdump : string -> string

(** Exponential Weighted Moving Average
  (smooth) 0.05 < alpha < 0.15 (dynamic)
*)
type ewma = (float -> unit) * (unit -> float)

(** [ewma alpha]

    @return [f_store, f_get] such that [f_store] is used to add a
    value to the EWMA and [f_get] returns the current EWMA including
    all the value already stored.
*)
val ewma : float -> ewma

(** generates a string of n random bytes. *)
val random_bytes : int -> string

(** generates a string of n random ascii chars. *)
val random_ascii : int -> string
