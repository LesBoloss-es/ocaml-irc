let (>>=) = Lwt.bind

type t =
  { sockaddr : Lwt_unix.sockaddr ;
    socket : Lwt_unix.file_descr }

let make address port =
  let open Lwt_unix in
  let sockaddr = ADDR_INET (Unix.inet_addr_of_string address, port) in
  let socket = socket (Unix.domain_of_sockaddr sockaddr) SOCK_STREAM 0 in
  { sockaddr ; socket }

let connect sock =
  Lwt_unix.connect sock.socket sock.sockaddr

let rec output sock str ofs =
  let len = String.length str - ofs in
  Lwt_unix.write_string sock str ofs len
  >>= fun rlen ->
  if rlen = len then
    Lwt.return ()
  else
    output sock str (ofs + rlen)

let output sock str =
  output sock.socket str 0
