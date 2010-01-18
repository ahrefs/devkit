(** File system *)

open Prelude
open Control

let enum_dir d = Enum.from (fun () -> try Unix.readdir d with End_of_file -> raise Enum.No_more_elements)
let with_readdir dirname = bracket (Unix.opendir dirname) Unix.closedir

(** [f fd path rel] gets invoked for each file under [dirname] where
[fd] is a read-only [Unix.file_descr], [path] is full path and [rel] - path relative to [dirname] *)
let rec iter_names dirname f =
  let rec loop path rel =
  with_readdir path (fun d ->
    enum_dir d >>
    Enum.iter (function
      | "." | ".." -> ()
      | name ->
        let path = Filename.concat path name in
        match try Some (Unix.openfile path [Unix.O_RDONLY] 0) with _ -> None with 
        | None -> ()
        | Some fd ->
          bracket fd (suppress Unix.close) (fun fd ->
            let rel = Filename.concat rel name in
            match (Unix.fstat fd).Unix.st_kind with
            | Unix.S_REG -> f fd path rel
            | Unix.S_DIR -> loop path rel
            | _ -> ()
          )
      )
    )
  in loop dirname ""

let iter_files dirname f =
  iter_names dirname (fun fd path _ ->
    bracket (Unix.in_channel_of_descr fd) close_in_noerr (fun ch -> f path ch))

let open_out_append = open_out_gen [Open_wronly;Open_append;Open_creat;Open_text] 0o644

(*
let () =
  iter_files "/etc" (fun s _ -> print_endline s)
*)

