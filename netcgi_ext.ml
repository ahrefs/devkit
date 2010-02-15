(** *)

open Netcgi

open Prelude
open Control

let cgi_show_exn exn cgi =
  (cgi:>cgi)#set_header ~cache:`No_cache ~content_type:"text/plain" ~status:`Internal_server_error ();
  let out = IO.from_out_channel cgi#out_channel in
  IO.printf out "%s\n%s" (Exn.str exn) (Printexc.get_backtrace ())

let cgi_suppress_exn exn cgi =
  (cgi:>cgi)#set_header ~cache:`No_cache ~content_type:"text/plain" ~status:`Internal_server_error ();
  Log.self #warn ~exn "Netcgi_ext.suppress_exn";
  cgi#out_channel#output_string "Internal server error"

let perform_cgi f (err:exn->cgi->unit) =
  fun cgi ->
  try
    let () = f (cgi:>cgi) in
    cgi#out_channel#commit_work ();
  with e ->
    cgi#out_channel#rollback_work ();
    err e (cgi:>cgi);
    cgi#out_channel#commit_work ()

class type cgi_args = 
object
method argument : string -> < value : string >
end

module Cgi_arg(T : sig val cgi : cgi_args end) =
struct
  let arg name = (T.cgi#argument name)#value
  exception Bad of string
  let get name = Exn.catch arg name
  let str name = match get name with Some s -> s | None -> raise (Bad name)
  let int name = let s = str name in try int_of_string s with _ -> raise (Bad name)
end

let noclose io =
  IO.create_out
    ~write:(IO.write io)
    ~output:(IO.output io)
    ~flush:(fun () -> IO.flush io)
    ~close:(fun () -> ())

let cgi_output cgi (f : 'a IO.output -> unit) =
  let out = IO.from_out_channel cgi#out_channel in (* not closing *)
  f (noclose out)

let serve_content cgi ?status ~ctype (f : 'a IO.output -> unit) =
  (cgi:>cgi)#set_header ~cache:`No_cache ~content_type:ctype ?status ();
  cgi_output cgi f

let serve_text_io cgi ?status = 
  serve_content cgi ?status ~ctype:"text/plain"

let serve_gzip_io cgi ?status f =
  serve_content cgi ?status ~ctype:"application/gzip" (fun io -> 
    Control.with_output (Gzip_io.output io) f)

let serve_text cgi ?status text = 
  serve_text_io cgi ?status (flip IO.nwrite text)

let serve_html cgi html =
  serve_content cgi ~ctype:"text/html" (fun out -> XHTML.M.pretty_print (IO.nwrite out) html)

let not_found cgi = serve_text cgi ~status:`Not_found "Not found"
let bad_request ?(text="Bad request") cgi = serve_text cgi ~status:`Bad_request text

(*
  let name = "/tmp/scraper.fcgi" in
  (try Unix.unlink name with _ -> ());
  Unix.umask 0o000 >> ignore; (* awful... *)
*)

let listen_fcgi port perform =
  let addr = Unix.inet_addr_loopback in
  Log.self #info "Ready for FastCGI on %s:%u" (Unix.string_of_inet_addr addr) port;
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  Netcgi_fcgi.run ~output_type:(`Transactional buffered)
    ~sockaddr:(Unix.ADDR_INET (addr,port)) perform

