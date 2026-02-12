(* client SSE implementation, Similar to EventSource object but differs where sensible
   https://html.spec.whatwg.org/multipage/server-sent-events.html#server-sent-events *)

type event = Comment of string | Event of (string * string) | Data of string
type ready_state = Connecting | Open | Closed

type t = {
  url : string;
  request : Curl.t;
  mutable reconnection_time : int; (* in miliseconds *)
  event : Buffer.t;
  data : Buffer.t;
  mutable last_event_id : string;
  mutable ready_state : ready_state;
}

module Parse = struct
  open Angstrom

  type ev = Comment of string | Field of (string * string)

  let pp_event fmt = function
    | Comment s -> Format.fprintf fmt "Comment %s" s
    | Field (f, s) -> Format.fprintf fmt "Field (%s, %s)" f s

  let char_list_to_string char_list =
    let len = List.length char_list in
    let bytes = Bytes.create len in
    List.iteri (Bytes.set bytes) char_list;
    Bytes.unsafe_to_string bytes

  (* Characters *)
  let lf' = '\x0A'
  let cr' = '\x0D'
  let colon' = ':'
  let space' = ' '

  (* Helper range checkers *)
  let is_any_char c = List.for_all (fun n -> not @@ Char.equal c n) [ lf'; cr' ]

  let is_name_char c =
    List.for_all (fun n -> not @@ Char.equal c n) [ lf'; cr'; colon' ]

  (* tokens *)
  let lf = char lf'
  let cr = char cr'
  let colon = char colon'
  let space = char space'
  let bom = string "\xFEFF"
  let any_char = satisfy is_any_char
  let name_char = satisfy is_name_char

  (* Rules *)
  let end_of_line =
    choice [ both cr lf *> return (); cr *> return (); lf *> return () ]

  let comment =
    lift3
      (fun _ comment _ -> Comment (char_list_to_string comment))
      colon (many any_char) end_of_line
    <?> "comment"

  let field =
    lift3
      (fun name value _ ->
        Field (char_list_to_string name, char_list_to_string value))
      (many1 name_char)
      (option [] (colon *> option space' space *> many any_char))
      end_of_line
    <?> "field"

  let event =
    many
      (choice ~failure_msg:"Couln't parse comment or field" [ field; comment ])
    <* end_of_line <?> "event"

  let stream = option "" bom *> map (many event) ~f:List.flatten <?> "stream"

  (* Parse *)
  let parse_string = Angstrom.parse_string ~consume:Prefix stream

  let parse_string_debug s =
    match parse_string s with
    | Ok result ->
        let pp_event_list ppf =
          Format.(pp_print_list ~pp_sep:pp_print_cut pp_event ppf)
        in
        Format.printf "@[Parsed successfully: @[<v>%a@]@]@." pp_event_list
          result
    | Error msg -> Printf.printf "Parsing failed: %s\n" msg

  let interpret_event t : ev -> event option = function
    | Comment s -> Some (Comment s)
    | Field (field, data) -> (
        match field with
        | "event" -> Some (Event (field, data))
        | "data" -> Some (Data data)
        | "id" ->
            if data.[0] <> '\x00' then t.last_event_id <- data;
            None
        | "retry" ->
            t.reconnection_time <- int_of_string data;
            None
        | f ->
            Printf.eprintf "Got unknown field \"%s\", ignoring\n" f;
            None)
end

let make ?(reconnection_time = 3000) ?(max_reconnect_attempt = 3)
    ?(headers = []) ?body ?(event_callback = ignore)
    ?(comment_callback = ignore) ~url callback =
  let t =
    {
      url;
      request = Curl.init ();
      reconnection_time;
      event = Buffer.create 10;
      data = Buffer.create 4096;
      last_event_id = "";
      ready_state = Connecting;
    }
  in
  Curl.setopt t.request (CURLOPT_MAXREDIRS max_reconnect_attempt);
  Curl.set_httpheader t.request ("Accept" :: "text/event-stream" :: headers);
  Option.may
    (fun body ->
      Curl.set_postfields t.request body;
      Curl.set_postfieldsize t.request (String.length body))
    body;
  Curl.set_url t.request url;
  Curl.set_writefunction t.request (fun chunk ->
      (match Angstrom.parse_string ~consume:Prefix Parse.stream chunk with
      | Ok data ->
          List.iter
            (fun ev ->
              match Parse.interpret_event t ev with
              | Some (Comment s) -> comment_callback s
              | Some (Event pair) -> event_callback pair
              | Some (Data d) -> callback d
              | None -> ())
            data
      | Error e -> Printf.eprintf "Parse error: %s" e);
      String.length chunk);

  (* Reconnection logic *)
  let rec perform_with_reconnect n =
    let%lwt curlCode = Curl_lwt.perform t.request in
    let code = Curl.int_of_curlCode curlCode in
    match code / 100 with
    | 2 ->
        t.ready_state <- Closed;
        Lwt.return_unit
    | _ ->
        Printf.eprintf "Connection broken: %d" code;
        if n <= 0 then (
          Printf.eprintf
            "Exceeded maximum connection retries, closing connection...";
          Lwt.return_unit)
        else (
          Printf.eprintf "Attempting to reconnect after %d ms"
            t.reconnection_time;
          let%lwt () =
            Lwt_unix.sleep (float_of_int (t.reconnection_time / 1000))
          in
          (* convert to seconds *)
          perform_with_reconnect (n - 1))
  in
  Lwt.async (fun () -> perform_with_reconnect max_reconnect_attempt);
  t

let ready_state { ready_state; _ } = ready_state

let close t =
  Curl.cleanup t.request;
  t.ready_state <- Closed
