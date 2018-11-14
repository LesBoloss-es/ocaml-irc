open Irc_common open Irc_model

type t =
  { socket : Lwt_unix.file_descr ;
    ichan : Lwt_io.input_channel ;
    ochan : Lwt_io.output_channel }

let make ~socket =
  { socket ;
    ichan = Lwt_io.of_fd ~mode:Input socket ;
    ochan = Lwt_io.of_fd ~mode:Output socket }

let open_ ~address ~port =
  let sockaddr =
    Unix.(ADDR_INET (inet_addr_of_string address, port))
  in
  let socket =
    Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
  in
  try%lwt
    Lwt_unix.connect socket sockaddr >>= fun () ->
    (try Lwt_unix.set_close_on_exec socket
     with Invalid_argument _ -> ());
    Lwt.return (make ~socket)
  with
    exn ->
    Lwt_unix.close socket >>= fun () ->
    Lwt.fail exn

let sockaddr conn = Lwt_unix.getpeername conn.socket
let ichan conn = conn.ichan
let ochan conn = conn.ochan

let host conn =
  match sockaddr conn with
  | Unix.ADDR_UNIX _ -> assert false
  | Unix.ADDR_INET (addr, _) -> Unix.string_of_inet_addr addr

let equal conn1 conn2 = conn1.socket == conn2.socket
let hash conn =
  (* Do not ever hash Lwt_unix.file_descr! *)
  Hashtbl.hash (Lwt_unix.unix_file_descr conn.socket)

let send conn message =
  Log.(debug (spf "<< %s" (Message.show message))) >>= fun () ->
  let string = Message.to_string message in
  Log.(debug (spf "<< %s" string)) >>= fun () ->
  Lwt_io.write (ochan conn) (string ^ "\n") (* FIXME: newline? *)

let send_l conn messages =
  Lwt_list.iter_s (send conn) messages

let send_async conn message =
  Lwt.async (fun () -> send conn message)

let send_async_l conn messages =
  List.iter (send_async conn) messages

let message_from_string string =
  let message = Message.from_string string in
  Log.(debug (spf ">> %s" string)) >>= fun () ->
  Log.(debug (spf ">> %s" (Result.show Message.show message))) >>= fun () ->
  Lwt.return message

let receive conn =
  Lwt_io.read_line (ichan conn) >>= message_from_string

let receive_stream conn =
  Lwt_io.read_lines (ichan conn)
  |> Lwt_stream.map_s message_from_string

module HashedSelf = struct
  type s = t
  type t = s

  let equal = equal
  let hash = hash
end

module Table = struct
  include Hashtbl.Make(HashedSelf)

  let to_list table =
    fold (fun k v l -> (k, v) :: l) table []

  let with_ table key try_ with_ =
    try try_ (find table key)
    with Not_found -> with_ ()
end
