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
  let message = Message.to_string message in
  Log.(debug_async (spf "<< %s" message));
  Lwt_io.write conn.ochan (message ^ "\n") (* FIXME: newline? *)

let send_async conn message =
  Lwt.async (fun () -> send conn message)

let receive conn =
  Lwt_io.read_line conn.ichan >>= fun message ->
  Log.(debug (spf ">> %s" message)) >>= fun () ->
  Lwt.return (Message.from_string message)
