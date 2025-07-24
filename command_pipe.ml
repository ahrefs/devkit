open Printf
open Devkit

module M = Map.Make (String)

let log = Log.from "command_pipe"

type t = {
  mutable status : [ `Stopped | `Running of Lwt_io.input Lwt_io.channel ];
  fname : string;
  mutable commands : (unit -> unit Lwt.t) M.t;
}

let kill t =
  match t.status with
  | `Stopped -> Lwt.return_unit
  | `Running ic -> begin
    t.status <- `Stopped;
    let%lwt () =
      try%lwt Lwt_unix.unlink t.fname
      with exn ->
        log#warn ~exn "unlink %S" t.fname;
        Lwt.return_unit
    in
    Lwt_io.close ic
  end

let make () =
  let dir_name = "var" in
  let fname = Filename.concat dir_name (sprintf "control.%d.fifo" (Unix.getpid ())) in
  (try Unix.unlink fname with _ -> ());
  let t = { status = `Stopped; fname; commands = M.empty } in
  let rec loop () =
    match t.status with
    | `Stopped -> Lwt.return_unit
    | `Running ic ->
      let%lwt () =
        match%lwt Lwt_io.read_line_opt ic with
        | None ->
          (* We need to reopen the FIFO because the reader gets EOF when the writer closes it. *)
          let%lwt () = Lwt_io.close ic in
          (* We explicitly provide the [flags] because we don't want the default [O_NONBLOCK] *)
          let%lwt ic = Lwt_io.open_file ~flags:[ O_RDONLY ] ~mode:Lwt_io.input fname in
          t.status <- `Running ic;
          Lwt.return_unit
        | Some command ->
        match M.find_opt (String.trim command) t.commands with
        | None ->
          let commands = M.bindings t.commands |> List.map fst |> List.map (sprintf "%S") |> String.concat ", " in
          log#error "command not found: %S. Registered commands: %s" command commands;
          Lwt.return_unit
        | Some f ->
        try%lwt f ()
        with exn ->
          log#error ~exn "command error %S" command;
          Lwt.return_unit
      in
      loop ()
  in
  let run () =
    try%lwt
      let%lwt () =
        try%lwt Lwt_unix.mkdir dir_name 0o744 with
        | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_unit
        | exn -> Lwt.reraise exn
      in
      let%lwt ic =
        let%lwt () = Lwt_unix.mkfifo fname 0o644 in
        Lwt_io.open_file ~flags:[ O_RDONLY; O_NONBLOCK ] ~mode:Lwt_io.input fname
      in
      t.status <- `Running ic;
      loop ()
    with exn ->
      log#error ~exn "run error";
      kill t
  in
  let bg_pool = Background_pool.create () in
  Background_pool.add ~at_exit:(fun () -> kill t) ~pick:(Daemon.wait_exit ()) bg_pool "command_pipe" (fun () -> run ());
  t

let add_command t name f = t.commands <- M.add name f t.commands
