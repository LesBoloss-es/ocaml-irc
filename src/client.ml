open Irc_common
module Conn = Irc_helpers.Connection

type config =
  { nicks : string list ;
    user : string ;
    realname : string ;
    address : string ;
    port : int }

class virtual skeleton config = object (self)
  inherit Irc_helpers.Handler.handler

  method loop conn : unit Lwt.t =
    let%lwt message = Conn.receive conn in
    (match message with
     | Ok message -> self#on_message conn message
     | Error () -> ()); (* FIXME: better than that *)
    self#loop conn

  method start _conn =
    Lwt.return ()

  method build () =
    let%lwt conn = Conn.open_ ~address:config.address ~port:config.port in
    self#start conn >>= fun () ->
    self#loop conn

  method run_async () : unit =
    Lwt.async (fun () -> self#build ())

  method run () : unit =
    Lwt_main.run (self#build ())
end

open Irc_model
open Irc_helpers.Command

class generic config = object
  inherit skeleton config as skeleton

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

  method on_ping conn _ server1 server2 =
    Conn.send_async conn (pong server1 server2)

  method on_pong _ _ _ _ = ()

  method on_nosuchnick _ _ _ = ()

  method! start conn =
    Conn.send conn (nick (Nickname.from_string (List.hd config.nicks))) >>= fun () ->
    Conn.send conn (user config.user 0 config.realname) >>= fun () ->
    skeleton#start conn
end
