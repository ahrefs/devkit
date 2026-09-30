(** Thread and domain utilities with no devkit dependencies, see {!ExtThread} *)

(** [check_main_domain name] raises [Failure] if not called from the main domain.
    Use it to guard process-wide setup and configuration. [name] identifies the caller in the error. *)
let check_main_domain name =
  if (Domain.self () :> int) <> 0 then
    failwith (name ^ ": must be called from the main domain")
