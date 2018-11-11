open Irc_common open Irc_model

type t =
  { sockaddr : Unix.sockaddr ;
    ichan : Lwt_io.input_channel ;
    ochan : Lwt_io.output_channel }

let open_ ~address ~port =
  let sockaddr =
    Unix.(ADDR_INET (inet_addr_of_string address, port))
  in
  let%lwt (ichan, ochan) = Lwt_io.open_connection sockaddr in
  Lwt.return { sockaddr ; ichan ; ochan }

let send conn message =
  Log.(debug (spf "<< %s" (Message.show message))) >>= fun () ->
  let string = Message.to_string message in
  Log.(debug (spf "<< %s" string)) >>= fun () ->
  Lwt_io.write conn.ochan (string ^ "\n") (* FIXME: newline? *)

let send_async conn message =
  Lwt.async (fun () -> send conn message)

let rec receive conn =
  Lwt_io.read_line conn.ichan >>= fun string ->
  Log.(debug (spf ">> %s" string)) >>= fun () ->
  try
    let message = Message.from_string string in
    Log.(debug (spf ">> %s" (Message.show message))) >>= fun () ->
    Lwt.return message
  with
    Invalid_argument _ ->
    Log.(warning (spf "Could not parse this message")) >>= fun () ->
    receive conn
