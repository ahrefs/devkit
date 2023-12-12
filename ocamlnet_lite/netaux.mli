(** Internal auxiliary functions 
 *
 * This is an internal module.
 *)

(* Auxiliary stuff *)

module ArrayAux : sig
  val int_blit : int array -> int -> int array -> int -> int -> unit
    (** A specialisation of [Array.blit] for int arrays. 
     * (Performance reasons.)
     *)

  val int_series : int array -> int -> int array -> int -> int -> int -> unit
    (** [int_series src srcpos dst dstpos len n]:
     * Computes for every [i], [0 <= i < len]:
     * [dst.(dstpos+i) = n + SUM(j=0..(i-1): src.(srcpos+j)) ]
     *
     * It is expected that [src == dst] implies [srcpos >= dstpos].
     *)

    (**/**)
    
  val int_blit_ref : 
    (int array -> int -> int array -> int -> int -> unit) ref
    (* Used by [Netaccel] to override the built-in implementation *)

  val int_series_ref : 
    (int array -> int -> int array -> int -> int -> int -> unit) ref
    (* Used by [Netaccel] to override the built-in implementation *)
end
