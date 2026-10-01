open Devkit
open QCheck2
module C = Cache.Count

(* each domain adds every key of [keys] once *)
let concurrent_add =
  Test.make ~name:"concurrent add" ~count:50
    Gen.(pair (int_range 1 8) (list_size (int_range 0 20) (string_size (int_range 0 3))))
    (fun (n_domains, keys) ->
      let c = C.create () in
      List.init n_domains (fun _ -> Domain.spawn (fun () -> List.iter (C.add c) keys))
      |> List.iter Domain.join;
      let expected = Hashtbl.create 16 in
      List.iter (fun k -> Hashtbl.replace expected k (n_domains + Option.value ~default:0 (Hashtbl.find_opt expected k))) keys;
      C.size c = Hashtbl.length expected
      && Hashtbl.fold (fun k n ok -> ok && C.count c k = n) expected true
      && C.count_all c = n_domains * List.length keys)

let nan_key =
  Test.make ~name:"nan key" ~count:1 Gen.unit (fun () ->
    let c = C.create () in
    C.add c nan; C.add c nan;
    C.size c = 1 && C.count c nan = 2)

let () =
  ignore (Unix.alarm 300 : int);
  exit (QCheck_base_runner.run_tests ~verbose:true [ concurrent_add; nan_key ])
