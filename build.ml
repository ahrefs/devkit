
let cmd cmd = Control.bracket (Unix.open_process_in cmd) (fun ch -> ignore @@ Unix.close_process_in ch) input_line

let git_describe () =
  let version = cmd "git describe --long --always --dirty=+\"$(git config user.name)@$(hostname)\"" in
  let version = String.map (function ' ' -> '.' | c -> c) version in
  match cmd "git symbolic-ref -q --short HEAD" with
  | exception End_of_file -> version
  | "" | "master" -> version
  | branch -> version^"-"^branch

let save_version outfile =
  Control.with_open_out_txt outfile begin fun out ->
    let revision = try git_describe () with _ -> "<unknown>" in
    Printf.fprintf out "let id = %S\n" revision
  end
