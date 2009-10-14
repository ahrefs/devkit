
module type WorkerT = sig
type task
type result
end

module Workers(T:WorkerT) = struct

let worker (execute : T.task -> T.result) =
  let main_read, child_write = Unix.pipe () in
  let child_read, main_write = Unix.pipe () in
  match Unix.fork() with
  | -1 -> assert false
  | 0 -> (* child *)
      Unix.close main_read; Unix.close main_write;
      let output = Unix.out_channel_of_descr child_write in
      let input = Unix.in_channel_of_descr child_read in
      begin try
      while true do
(*         print_endline "read"; *)
        let (r,_,e) = Nix.restart (fun () -> Unix.select [child_read] [] [child_read] (-1.)) () in
        assert (e=[]);
        assert (r<>[]);
(*         print_endline "will read"; *)
        let v = (Marshal.from_channel input : T.task) in
(*         print_endline "read done"; *)
        let r = execute v in
        Marshal.to_channel output (r : T.result) []; flush output
      done
      with e -> Printf.printf "exception %s\n%!" (Printexc.to_string e)
      end;
      close_in_noerr input;
      close_out_noerr output;
      exit 0
  | pid ->
      Unix.close child_read; Unix.close child_write;
      let output = Unix.out_channel_of_descr main_write in
      let input = Unix.in_channel_of_descr main_read in
      input,output,pid

type t = (in_channel * out_channel * int) list * (T.task -> T.result)

let create f n = let l = ref [] in for i = 1 to n do l := worker f :: !l done; !l, f

let perform (l,execute) e f =
    match l with
    | [] -> Enum.iter (fun x -> f (execute x)) e (* no workers *)
    | l ->
      let workers = ref 0 in
      List.iter (fun (i,o,p) ->
        match Enum.get e with
        | None -> ()
        | Some x -> incr workers; Marshal.to_channel o ( x : T.task) []; flush o) l;
(*       Printf.printf "workers %u\n%!" !workers; *)
      while !workers > 0 do
        let fdl = List.map (fun (i,_,_) -> Unix.descr_of_in_channel i) l in
        let (r,_,err) = Nix.restart (fun () -> Unix.select fdl [] fdl (-1.)) () in
(*         print_endline "select done"; *)
        assert (err = []);
        let channels = List.map (fun fd -> let (i,o,_) = List.find (fun (i,_,_) -> Unix.descr_of_in_channel i = fd) l in i,o) r in
        let answers = List.map (fun (r,w) ->
          let task = Enum.get e in
          let answer = (Marshal.from_channel r : T.result) in
          begin match task with
          | None -> decr workers
          | Some x -> Marshal.to_channel w ( x : T.task) []; flush w
          end; answer) channels in
        List.iter f answers
      done

end

