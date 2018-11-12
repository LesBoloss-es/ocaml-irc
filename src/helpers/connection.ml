open Irc_common open Irc_model

type t =
  { sockaddr : Unix.sockaddr ;
    ichan : Lwt_io.input_channel ;
    ochan : Lwt_io.output_channel }

let make ~sockaddr ~ichan ~ochan =
  { sockaddr ; ichan ; ochan }

let open_ ~address ~port =
  let sockaddr =
    Unix.(ADDR_INET (inet_addr_of_string address, port))
  in
  let%lwt (ichan, ochan) = Lwt_io.open_connection sockaddr in
  Lwt.return (make ~sockaddr ~ichan ~ochan)

let send conn message =
  Log.(debug (spf "<< %s" (Message.show message))) >>= fun () ->
  let string = Message.to_string message in
  Log.(debug (spf "<< %s" string)) >>= fun () ->
  Lwt_io.write conn.ochan (string ^ "\n") (* FIXME: newline? *)

let send_async conn message =
  Lwt.async (fun () -> send conn message)

let message_from_string string =
  let message = Message.from_string string in
  Log.(debug (spf ">> %s" string)) >>= fun () ->
  Log.(debug (spf ">> %s" (Result.show Message.show message))) >>= fun () ->
  Lwt.return message

let receive conn =
  Lwt_io.read_line conn.ichan >>= message_from_string

let receive_stream conn =
  Lwt_io.read_lines conn.ichan
  |> Lwt_stream.map_s message_from_string

module HashedSelf = struct
  type s = t
  type t = s

  let equal c1 c2 =
    c1.sockaddr = c2.sockaddr

  let hash c =
    Hashtbl.hash c
end

module Table = Hashtbl.Make(HashedSelf)
