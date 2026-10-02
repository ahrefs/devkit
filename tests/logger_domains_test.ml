open Devkit
open QCheck2

let sink counter = fun _level _facil _s -> Atomic.incr counter

let mk_target counter =
  { Logger.format = (fun _level _facil _ts _pairs msg -> msg); output = Atomic.make (sink counter) }

(* [n_domains] domains each log [n_msgs] times while the main domain
   runs [reconfigure] until they are done *)
let run ~reconfigure ~facil ~target n_domains n_msgs =
  let logger = Logger.put_simple target in
  let done_ = Atomic.make 0 in
  let ds = List.init n_domains (fun _ -> Domain.spawn (fun () ->
    for i = 1 to n_msgs do
      logger.Logger.put `Info facil 0. [] (string_of_int i)
    done;
    Atomic.incr done_))
  in
  let i = ref 0 in
  while Atomic.get done_ < n_domains do
    reconfigure !i; incr i; Domain.cpu_relax ()
  done;
  List.iter Domain.join ds

let gen = Gen.(pair (int_range 1 8) (int_range 0 2000))

(* swapping outputs never loses nor duplicates a message *)
let swap_output =
  Test.make ~name:"swap output" ~count:30 gen (fun (n_domains, n_msgs) ->
    let a = Atomic.make 0 and b = Atomic.make 0 in
    let target = mk_target a in
    let facil = { Logger.name = "test"; show = Atomic.make (Logger.int_level `Debug) } in
    run ~facil ~target n_domains n_msgs
      ~reconfigure:(fun i -> Atomic.set target.output (sink (if i land 1 = 0 then b else a)));
    Atomic.get a + Atomic.get b = n_domains * n_msgs)

(* toggling the filter concurrently with logging *)
let toggle_filter =
  Test.make ~name:"toggle filter" ~count:30 gen (fun (n_domains, n_msgs) ->
    let a = Atomic.make 0 in
    let target = mk_target a in
    let facil = { Logger.name = "test"; show = Atomic.make (Logger.int_level `Debug) } in
    run ~facil ~target n_domains n_msgs
      ~reconfigure:(fun i -> Logger.set_filter facil (if i land 1 = 0 then `Nothing else `Debug));
    Logger.set_filter facil `Error;
    Atomic.get a <= n_domains * n_msgs && Logger.get_level facil = `Error)

let () =
  ignore (Unix.alarm 300 : int);
  exit (QCheck_base_runner.run_tests ~verbose:true [ swap_output; toggle_filter ])
