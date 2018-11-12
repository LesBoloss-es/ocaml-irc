(* open Irc_common *)
module Conn = Irc_helpers.Connection

type config =
  { listen_address : string ;
    listen_port : int }

class virtual skeleton config = object (self)
  inherit Irc_helpers.Handler.handler

  method on_connection conn =
    Conn.receive_stream conn
    |> Lwt_stream.iter
         (function
          | Ok message -> self#on_message conn message
          | Error () -> ()) (* FIXME: better than that *)

  method build () =
    let listen_address =
      match config.listen_address with
      | "any" -> Unix.inet_addr_any
      | _ -> Unix.inet_addr_of_string config.listen_address
    in
    let my_sockaddr = Unix.ADDR_INET (listen_address, config.listen_port) in
    Lwt_io.establish_server_with_client_address
      my_sockaddr
      (fun sockaddr (ichan, ochan) ->
        let conn = Conn.make ~sockaddr ~ichan ~ochan in
        self#on_connection conn)

  method run_async () : Lwt_io.server =
    Lwt_main.run (self#build ())
end

(* open Irc_model
 * open Irc_helpers.Command *)

class generic config = object
  inherit skeleton config as skeleton

  val clients = Conn.Table.create 8

  method on_welcome _ _ _ _ = ()
  method on_yourhost _ _ _ _ = ()
  method on_created _ _ _ _ = ()
  method on_myinfo _ _ _ _ _ _ _ = ()
  method on_bounce _ _ _ _ = ()

  method on_motdstart _ _ _ _ = ()
  method on_motd _ _ _ _ = ()
  method on_endofmotd _ _ _ _ = ()

  method on_pass _ _ _ = ()
  method on_join _ _ _ = ()
  method on_privmsg _ _ _ _ = ()
  method on_notice _ _ _ _ = ()
  method on_ping _ _ _ _ = ()
  method on_pong _ _ _ _ = ()

  method on_nosuchnick _ _ _ = ()

  method! on_connection conn =
    Conn.Table.add clients conn ();
    skeleton#on_connection conn
end
