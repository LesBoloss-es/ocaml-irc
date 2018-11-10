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

let send_lwt conn message =
  let string = Message.to_string message in
  Log.(debug_async (spf "<< %s" string));
  Lwt_io.write conn.ochan (string ^ "\n") (* FIXME: newline? *)

let send conn message =
  Lwt.async (fun () -> send_lwt conn message)

let receive conn =
  Lwt_io.read_line conn.ichan >>= fun string ->
  Log.(debug_async (spf ">> %s" string));
  let message = Message.from_string string in
  Lwt.return message

let send_nick conn nick =
  send conn (Message.from_command (Command.Nick nick))

let send_user conn user mode realname =
  send conn (Message.from_command (Command.User (user, mode, realname)))
