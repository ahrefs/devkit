open ExtLib
open Prelude

let last_logs_max = 10

let enabled = ref false

let is_enabled () = !enabled

(**)

module LastN = struct

  type 'a t =
    { queue : 'a Queue.t;
      mutable avail : int;
    }

  let create n =
    if n < 0 then invalid_arg "LastN.create: n < 0" else
    { queue = Queue.create (); avail = n }

  let add x t =
    Queue.push x t.queue;
    if t.avail = 0 then
      ignore (Queue.pop t.queue)
    else
      t.avail <- t.avail - 1

  let to_list t =
    List.rev @@ Queue.fold (fun acc x -> x :: acc) [] t.queue

end

(**)

type id = int

type kind =
  | Normal
  | Background
  | Status

type lazy_string = string Lazy.t

type mark =
  { id : id;
    kind : kind;
    name : lazy_string;
    parent_name : lazy_string;
    parent_id : id;
      (** [id] is stored to find parent thread in !marks, but there are no direct links to parent's mark.
          [parent_{name,id}] don't reflect Lwt scheduling (so background thread's parent is not set to main/unnamed/toplevel); they are
          used to trace places where threads were born (control flow). *)
    logs : lazy_string LastN.t;
  }

(**)

let string_of_kind = function
  | Normal -> "normal"
  | Background -> "background"
  | Status -> "status"

(** [0] is a special value, not used by threads. *)
let next_mark_id = ref 1

module Int = struct
  type t = int
  let compare (x : int) y = Pervasives.compare (x : int) y
end
module IntMap = Map.Make(Int)

let marks = ref IntMap.empty

let create ~name ~parent_id ~parent_name ~kind =
  { id = (let id = !next_mark_id in next_mark_id := id + 1; id);
    name;
    parent_id;
    parent_name;
    logs = LastN.create last_logs_max;
    kind;
  }

let register_mark m =
  match IntMap.find m.id !marks with
  | exception Not_found -> marks := IntMap.add m.id m !marks
  | _ -> assert false

let unregister_mark m =
  match IntMap.find m.id !marks with
  | _ -> marks := IntMap.remove m.id !marks
  | exception Not_found -> assert false

let special name =
  let m = create ~name:(Lazy.from_val name) ~parent_id:0 ~parent_name:(Lazy.from_val "") ~kind:Normal in
  register_mark m;
  m

(** dummy parent of threads created by parents without mark *)
let top_mark = special "<top>"

(** dummy parent of threads/statuses which parent has terminated *)
let orphan_mark = special "<orphan>"

(**)

let log_add_line mark msg =
  let msg = lazy begin
      let msg = !!msg in
      if String.ends_with msg "\n" then msg else msg ^ "\n"
    end
  in
  LastN.add msg mark.logs

let log_to mark msg =
  if not !enabled then () else
  log_add_line mark msg

let key = Lwt.new_key ()

let with_mark v f =
  Lwt.with_value key v f

let run_thread on_success on_failure func =
  match func () with
  | thr -> Lwt.on_any thr on_success on_failure; thr
  | exception exn -> on_failure exn; Lwt.fail exn

let mark_or_orphan id =
  try IntMap.find id !marks with Not_found -> orphan_mark

let log_exit mark msg =
  let parent = mark_or_orphan mark.parent_id in
  log_to parent begin
    let {name; id; kind; parent_name; parent_id; logs = _} = mark in
    lazy begin
      Printf.sprintf "thread %S (#%i, %s%s) exit %s\n"
        !!name id (string_of_kind kind)
        (if parent == orphan_mark then Printf.sprintf ", parent was %s#%i" !!parent_name parent_id else "")
        !!msg
    end
  end

(** separate function to ease reasoning about which values are kept in closures (here: only arguments and top-level values, no local
    bindings from [with_new_mark]) *)
let run_with_mark ?dump ?log:(log : Log.logger option) ~mark cont () =
  register_mark mark;
  let on_success v =
    unregister_mark mark;
    log_exit mark @@ lazy begin
      "ok" ^ (match dump with None -> "" | Some dump -> ", res: " ^ dump v)
    end;
  in
  let on_failure exn =
    unregister_mark mark;
    log_exit mark @@ lazy begin
      "exn: " ^ Printexc.to_string exn
    end;
    begin match log with None -> () | Some log -> log #warn "thread %S failed" !!(mark.name) ~exn end;
  in
  run_thread on_success on_failure cont

let with_new_mark ?dump ?log ~name ~kind cont =
  if not !enabled then cont () else
  let new_mark =
    let (parent_name, parent_id) =
      let parent = Option.default top_mark (Lwt.get key) in
      (parent.name, parent.id)
    in
      create ~name ~kind ~parent_name ~parent_id
  in
  with_mark (Some new_mark) @@ run_with_mark ?dump ?log ~mark:new_mark cont

(**)

let name name cont =
  with_new_mark ~name:(Lazy.from_val name) ~kind:Normal cont

let status name ?dump cont =
  with_new_mark ~name ?dump ~kind:Status cont

let status_s name ?dump cont =
  status (Lazy.from_val name) ?dump cont

let async ?log name run_thread =
  Lwt.async @@ fun () ->
    with_new_mark ?log ~name:(Lazy.from_val name) ~kind:Background @@
    run_thread

let ignore_result = async

let log_do msg =
  let mark = Option.default top_mark (Lwt.get key) in
  log_add_line mark msg

let log_l msg =
  if not !enabled then () else
  log_do msg

let log_do_strict msg =
  log_do (Lazy.from_val msg)

let log msg =
  if not !enabled then () else
  log_do_strict msg

let log_f fmt =
  if not !enabled then Printf.ikfprintf ignore () fmt else Printf.ksprintf log_do_strict fmt

(**)

let rec parent_of_status parent_id =
  let parent = mark_or_orphan parent_id in
  match parent.kind with
  | Normal | Background -> parent
  | Status -> parent_of_status parent.parent_id

let summary () =
  let b = Buffer.create 100 in
  let open Printf in
  Buffer.add_string b "Lwt_mark status (running threads):\n";
  if !enabled
  then begin
    let statuses = IntMap.fold begin fun _id mark acc ->
        match mark.kind with
        | Normal | Background -> acc
        | Status -> begin
            let {id = parent_id; _} = parent_of_status mark.parent_id in
            let (acc, statuses) =
              try (acc, IntMap.find parent_id acc)
              with Not_found -> let s = ref [] in (IntMap.add parent_id s acc, s)
            in
            tuck statuses mark;
            acc
          end
      end
      !marks
      IntMap.empty
    in
    IntMap.iter begin fun _id {id; name; parent_id; parent_name; logs; kind} ->
        bprintf b "%s (#%i, %s%s)\n"
          !!name id (string_of_kind kind)
          (if parent_id = 0 then "" else sprintf ", parent: %s#%i" !!parent_name parent_id);
        let logs = LastN.to_list logs in
        List.iter (fun line -> Buffer.add_string b " L "; Buffer.add_string b !!line) logs;
        begin match kind with
          | Status -> ()
          | Normal | Background ->
              let sts =
                match IntMap.find id statuses with
                | sts_acc -> List.rev !sts_acc
                | exception Not_found -> []
              in
              List.iter (fun status -> bprintf b " S %s#%i\n" !!(status.name) status.id) sts
        end;
        Buffer.add_char b '\n'
      end
      !marks
  end else
    bprintf b "<not initialized>\n";
  Buffer.contents b

(**)

let init () =
  enabled := true;
  let old_hook = !Log.State.hook in
  Log.State.hook := fun level facil msg -> (log msg; old_hook level facil msg)
