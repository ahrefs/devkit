open Printf
open Prelude
open Httpev_common

type t = { mutable stamp : Time.t; mutable index : int; realm : string; user : string; password : string; }

type digest_request = {
    name:string;
    crealm:string;
    nonce:string;
    uri:string;
    qop:[`Auth | `Authi | `Unknown];
    nc:string;
    cnonce:string;
    response:string;
    opaque:string;
}

module Parse = struct (* awful *)

let appendlst lst elem = 
   lst := List.append !lst [elem]

let appendstr str elem =
  str := ((!str) ^ elem)

let lowparse elem curstr curlist = 
  if elem = ',' then begin
    if (String.length !curstr) > 0 then begin
    appendlst curlist !curstr ; end;
    curstr:=""; 
  end else if (elem <>  ' ')&&(elem <> '"')&&(elem<>'\n')&&(elem<>'\r') then appendstr curstr (Char.escaped elem)

let make_tuple a b = (a,b)

let highparse str curlist  =
  let first_equal = try String.index str '='with Not_found -> Exn.fail "symbol = not found in %s" str in
  appendlst curlist (make_tuple (String.sub str 0 first_equal) (String.sub str (first_equal+1) (String.length(str)-1-first_equal)))

let digest_request_from_string s =
   if String.length s < 6 then Exn.fail "Digest string too short";
   let s1 = String.sub s 0 6 in
   if String.lowercase s1 <> "digest" then Exn.fail "Authorization fail - non-digest trying to connect";
   let str = String.sub s 6 ((String.length s) - 6) in
   let tmpstr = ref "" in
   let a = str^"," in
   let tmplist = ref [] in
   String.iter (fun a -> lowparse a tmpstr tmplist) a;
   let resultlist = ref [] in
   List.iter (fun a -> highparse a resultlist) !tmplist;
   let get k = try List.assoc k !resultlist with Not_found -> "" in
   {
      name     = get "username";
      crealm   = get "realm";
      nonce    = get "nonce";
      uri      = get "uri";
      qop      = (match get "qop" with "auth" -> `Auth | "auth-int" -> `Authi | _ -> `Unknown);
      nc       = get "nc";
      cnonce   = get "cnonce";
      response = get "response";
      opaque   = get "opaque";
   }

let _string_from_digest_request p =
   let s = "Digest username=\""^p.name^"\", realm=\""^p.crealm^"\", nonce=\""^p.nonce^"\", uri=\""^p.uri^"\", qop=" in
   let a = match p.qop with
           | `Auth -> s^"auth"
           | `Authi -> s^"auth-int"
           | `Unknown -> s^"unknown" in
   let a2 = a^", nc="^p.nc^", cnonce=\""^p.cnonce^"\", response=\""^p.response^"\", opaque=\""^p.opaque^"\"" in
   a2

end (* Parse *)

let md5_hex_string = Digest.(to_hex $ string)
let hash l = md5_hex_string @@ String.concat ":" l

let digest_opaque = md5_hex_string @@ Action.random_bytes 64

let init ~realm ~user ~password () = { realm; user; password; stamp = Time.now (); index = 1; }

let check t req =
  if Time.now () -. t.stamp > 300. then
  begin
    t.stamp <- Time.now ();
    t.index <- t.index + 1;
  end;
  let nonce = hash [Unix.string_of_inet_addr @@ client_ip req; string_of_float t.stamp; string_of_int t.index] in
  try 
     let dig = List.assoc "authorization" req.headers |> Parse.digest_request_from_string in
     match dig.nonce = nonce with
     | false -> raise Not_found
     | true -> (* Nonce is ok, checking another params *)
        let ha1 = hash [t.user; t.realm; t.password] in
        let ha2 = hash [show_method req.meth; dig.uri] in
        let response =
          match dig.qop with
          | `Authi |`Auth -> hash [ha1; dig.nonce; dig.nc; dig.cnonce; "auth"; ha2]
          | `Unknown -> hash [ha1; dig.nonce; ha2]
        in
        if dig.opaque <> digest_opaque || dig.response <> response then raise Not_found;
        `Ok
  with
  | _ ->
    let v = sprintf "Digest realm=\"%s\", qop=\"auth\", nonce=\"%s\", opaque=\"%s\"" t.realm nonce digest_opaque in
    `Unauthorized ("WWW-Authenticate", v)
