
type t =
  { ichan : Lwt_io.input_channel ;
    ochan : Lwt_io.output_channel }

let connect ~address ~port =
  let sockaddr =
    Unix.(ADDR_INET (inet_addr_of_string address, port))
  in
  let%lwt (ichan, ochan) = Lwt_io.open_connection sockaddr in
  Lwt.return { ichan ; ochan }

let send _ _ = assert false

let receive _ = assert false
