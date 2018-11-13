open Irc_common
module Conn = Irc_helpers.Connection

module type Config = sig
  val nicks : string list
  val user : string
  val realname : string
  val address : string
  val port : int
end

module type S = sig
  val build : unit -> unit Lwt.t
  val run_async : unit -> unit
  val run : unit -> unit
end

module FromHandler (C : Config) (H : Irc_helpers.Handler.Handler) : S = struct
  let rec loop conn : unit Lwt.t =
    let%lwt message = Conn.receive conn in
    (match message with
     | Ok message -> H.on_message conn message
     | Error () -> ()); (* FIXME: better than that *)
    loop conn

  let build () =
    let%lwt conn = Conn.open_ ~address:C.address ~port:C.port in
    H.on_connection conn;
    loop conn

  let run_async () : unit =
    Lwt.async (fun () -> build ())

  let run () : unit =
    Lwt_main.run (build ())
end

module FromEvents (C : Config) (E : Irc_helpers.Handler.Events) : S = FromHandler (C) (Irc_helpers.Handler.Make(E))

open Irc_model
open Irc_helpers.Command

module GenericEvents (C : Config) : Irc_helpers.Handler.Events = struct
  let on_connection conn =
    Lwt.async (fun () ->
        Conn.send conn (nick (Nickname.from_string (List.hd C.nicks))) >>= fun () ->
        Conn.send conn (user C.user 0 C.realname))

  let on_welcome _ _ _ _ = ()
  let on_yourhost _ _ _ _ = ()
  let on_created _ _ _ _ = ()
  let on_myinfo _ _ _ _ _ _ _ = ()
  let on_bounce _ _ _ _ = ()

  let on_motdstart _ _ _ _ = ()
  let on_motd _ _ _ _ = ()
  let on_endofmotd _ _ _ _ = ()

  let on_pass _ _ _ = ()
  let on_nick _ _ _ = ()
  let on_user _ _ _ _ _ = ()
  let on_join _ _ _ = ()
  let on_privmsg _ _ _ _ = ()
  let on_notice _ _ _ _ = ()

  let on_ping conn _ server1 server2 =
    Conn.send_async conn (pong server1 server2)

  let on_pong _ _ _ _ = ()

  let on_nosuchnick _ _ _ = ()
end

module GenericClient (C : Config) = FromEvents(C)(GenericEvents(C))
