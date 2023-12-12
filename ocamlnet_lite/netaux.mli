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
end
