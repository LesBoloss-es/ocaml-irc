open Irc.Common open Irc.Model
module Conn = Irc.Helpers.Connection
open Irc.Helpers.Command

let mirror config = object (_self)
  inherit Irc.Client.generic config

  method! on_welcome conn _ _ _ =
    Conn.send_async conn (join [Channel.from_string "#abcdefgh", None])

  method! on_privmsg conn prefix target content =
    let source =
      match unwrap prefix with
      | Identity identity -> Identity.nick identity
      | _ -> assert false
    in
    (
      match target with
      | All -> assert false
      | Channel channel -> privmsg (Channel channel) content
      | Nickname _ -> privmsg (Nickname source) content
    )
    |> Conn.send_async conn
end

let config =
  Irc.Client.{
      nicks = ["mirror"; "fallback_nickname"] ;
      user = "mexample" ;
      realname = "Mirror Example" ;
      address = "104.200.152.162" ; (* freenode *)
      port = 6667
  }

let () = Lwt_main.run ((mirror config) # build ())
