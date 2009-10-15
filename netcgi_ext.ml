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
  Exn.log exn "Netcgi_ext.suppress_exn";
  cgi#out_channel#output_string "Internal server error"

let perform_cgi f err =
  fun cgi ->
  try
    f (cgi:>cgi);
    cgi#out_channel#commit_work ();
  with e ->
    cgi#out_channel#rollback_work ();
    err e cgi;
    cgi#out_channel#commit_work ()

module Make_cgi(T : sig val cgi : Netcgi.cgi end) = 
struct

  let cgi = T.cgi

  let outs = cgi#out_channel#output_string

  module Arg = struct
    exception Bad of string
    let get name = try Some (cgi#argument name)#value with _ -> None
    let str name = match get name with Some s -> s | None -> raise (Bad name)
    let int name = let s = str name in try int_of_string s with _ -> raise (Bad name)
  end

  let serve_text_io ?status (f : 'a IO.output -> unit) =
    cgi#set_header ~cache:`No_cache ~content_type:"text/plain" ?status ();
    let out = IO.from_out_channel cgi#out_channel in (* not closing *)
    f out

  let serve_gzip_io ?status (f : 'a IO.output -> unit) =
    cgi#set_header ~cache:`No_cache ~content_type:"application/gzip" ?status ();
    outs (Gzip_io.pipe_in f)

  let serve_text ?status text = serve_text_io ?status (flip IO.nwrite text)

  let not_found () = serve_text ~status:`Not_found "Not found"
  let bad_request () = serve_text ~status:`Bad_request "Bad request"

end

