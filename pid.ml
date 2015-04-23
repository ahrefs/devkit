open ExtLib
open Printf

type t = { host : string; id : int; name : string; stamp : int; }

let dummy = { host="*"; id=0; name="*"; stamp=0; }

let show { id; name; host; stamp=_; } = sprintf "%u:%s@%s" id name host
let to_string { id; name; host; stamp; } = sprintf "%u:%u:%s@%s" stamp id name host
let compare (pid1:t) pid2 = compare pid1 pid2
let equal pid1 pid2 = 0 = compare pid1 pid2
let short_name { name; _ } = try fst @@ String.split name "." with _ -> name

let parse_exn s =
  (* cf self_pid *)
  Scanf.sscanf s "%u:%u:%[a-zA-Z0-9_.-]@@%[a-zA-Z0-9_-]%!" (fun stamp id name host ->
   if host = "" then Exn.fail "empty hostname";
   if name = "" then Exn.fail "empty name";
   { id; host=String.lowercase host; name=String.lowercase name; stamp; })

let self_stamp = ref None
let self_name = ref @@ Filename.basename Sys.executable_name

let self_as name =
  let id = Unix.getpid () in
  let stamp = match !self_stamp with
  | Some (pid,stamp) when pid = id -> stamp
  | _ -> let stamp = Time.(int @@ now ()) in self_stamp := Some (id,stamp); stamp
  in
  let host = String.lowercase @@ Unix.gethostname () in
  begin try Scanf.sscanf host "%_[a-zA-Z0-9_-]%!" () with _ -> Exn.fail "Pid.self: bad host %S" host end;
  (* cf parse_pid_exn *)
  begin try Scanf.sscanf name "%_[a-zA-Z0-9_.-]%!" () with _ -> Exn.fail "Pid.self: bad name %S" name end;
  { host; id; name=String.lowercase name; stamp; }

let set_name name = self_name := name

let self () = self_as !self_name
let show_self () = show @@ self ()
