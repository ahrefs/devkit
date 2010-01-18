(** Misc *)

open ExtLib
open Printf

open Prelude

let period n f = 
  let count = ref 0 in
  (fun () -> incr count; if !count mod n = 0 then f !count)

let strl f l = sprintf "[%s]" (String.concat ";" (List.map f l))

let uniq p e =
  let h = Hashtbl.create 16 in
  Enum.filter (fun x ->
    let k = p x in
    if Hashtbl.mem h k then false else (Hashtbl.add h k (); true)) e

let list_uniq p = List.of_enum $ uniq p $ List.enum

(** [partition l n] splits [l] into [n] chunks *)
let partition l n =
  let a = Array.make n [] in
  ExtList.List.iteri (fun i x -> let i = i mod n in a.(i) <- x :: a.(i)) l;
  a

module App(Info : sig val version : string val name : string end) = struct

let run main =
  Printexc.record_backtrace true;
  Log.info "%s started. Version %s. PID %u" Info.name Info.version (Unix.getpid ());
  try
    main ();
    Log.info "%s finished." Info.name
  with
    e -> Log.error "%s aborted : %s" Info.name (Exn.str e); Log.error_s (Printexc.get_backtrace ())

end

