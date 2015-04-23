open ExtLib

type t = Time of Time.t | Count of int | Bytes of int
type group = { name : string; k : string; get : (unit -> (string * t) list); }

let h_groups = Hashtbl.create 10
let register g = Hashtbl.replace h_groups g.name g

let make_cc f pp name k =
  let cc = Cache.Count.create () in
  let get () = Cache.Count.fold cc (fun k n acc -> (pp k, f n) :: acc) [] in
  register { name; k; get };
  cc

let cc f = make_cc (fun n -> Count n) f
let cc_ms f = make_cc (fun n -> Time (float n /. 1000.)) f

class typ name k =
object(self)
  val h = Hashtbl.create 7
  initializer
    let get () = Hashtbl.fold (fun k v acc -> (k, v ()) :: acc) h [] in
    register { name; k; get }
  method ref : 'a. 'a -> ('a -> t) -> string -> 'a ref = fun init f name ->
    let v = ref init in
    Hashtbl.replace h name (fun () -> f !v);
    v
  method count name = self#ref 0 (fun x -> Count x) name
  method bytes name = self#ref 0 (fun x -> Bytes x) name
  method time name = self#ref 0. (fun x -> Time x) name
end

let iter f =
  h_groups |> Hashtbl.iter begin fun _name g ->
    g.get () |> List.iter (fun (k,v) -> f g.name g.k k v)
  end

(*
let show () =
  let b = Buffer.create (Hashtbl.length h_vars * 20) in
  iter begin fun ~t ~k ~kname:_ ~v ->
    Printf.bprintf b "%s[%s]=%s " t k (match v with Int n -> string_of_int n | Float f -> string_of_float f);
  end;
  Buffer.contents b
*)
