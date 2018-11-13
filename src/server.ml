(* open Irc_common *)
module Conn = Irc_helpers.Connection

module type Config = sig
  val listen_address : string
  val listen_port : int
end

module type S = sig
  val on_connection : Conn.t -> unit Lwt.t
  val build : unit -> Lwt_io.server Lwt.t
  val run_async : unit -> Lwt_io.server
end

module Make (C : Config) (H : Irc_helpers.Handler.Handler) : S = struct

  let on_connection conn =
    H.on_connection conn;
    Conn.receive_stream conn
    |> Lwt_stream.iter
         (function
          | Ok message -> H.on_message conn message
          | Error () -> ()) (* FIXME: better than that *)

  let build () =
    let listen_address =
      match C.listen_address with
      | "any" -> Unix.inet_addr_any
      | _ -> Unix.inet_addr_of_string C.listen_address
    in
    let my_sockaddr = Unix.ADDR_INET (listen_address, C.listen_port) in
    Lwt_io.establish_server_with_client_address
      my_sockaddr
      (fun sockaddr (ichan, ochan) ->
        let conn = Conn.make ~sockaddr ~ichan ~ochan in
        on_connection conn)

  let run_async () : Lwt_io.server =
    Lwt_main.run (build ())
end

(* open Irc_model
 * open Irc_helpers.Command *)

module GenericEvents (C : Config) : Irc_helpers.Handler.Events = struct
  let clients = Conn.Table.create 8

  let on_connection conn =
    Conn.Table.add clients conn ()

  let on_welcome _ _ _ _ = ()
  let on_yourhost _ _ _ _ = ()
  let on_created _ _ _ _ = ()
  let on_myinfo _ _ _ _ _ _ _ = ()
  let on_bounce _ _ _ _ = ()

  let on_motdstart _ _ _ _ = ()
  let on_motd _ _ _ _ = ()
  let on_endofmotd _ _ _ _ = ()

  let on_pass _ _ _ = ()
  let on_join _ _ _ = ()
  let on_privmsg _ _ _ _ = ()
  let on_notice _ _ _ _ = ()
  let on_ping _ _ _ _ = ()
  let on_pong _ _ _ _ = ()

  let on_nosuchnick _ _ _ = ()
end
