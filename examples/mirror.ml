module Events (Config : Irc.Client.Config) = struct
  include Irc.Client.GenericEvents (Config)

  open Irc.Common
  open Irc.Model
  open Irc.Helpers.Command
  module Conn = Irc.Helpers.Connection

  let on_welcome conn _ _ _ =
    Conn.send_async conn (join [Channel.from_string "#abcdefgh", None])

  let on_privmsg conn prefix target content =
    let source =
      match unwrap prefix with
      | Prefix.Identity identity -> Identity.nick identity
      | _ -> assert false
    in
    (
      match target with
      | Target.All -> assert false
      | Target.Channel channel -> privmsg (Channel channel) content
      | Target.Nickname _ -> privmsg (Nickname source) content
    )
    |> Conn.send_async conn
end

let () =
  let module Config =
    struct
      let nicks = ["mirror"; "fallback_nickname"]
      let user = "mexample"
      let realname = "Mirror Example"

      let address = "104.200.152.162" (* freenode *)
      let port = 6667
    end
  in
  let module Mirror = Irc.Client.FromEvents(Config)(Events(Config)) in
  Mirror.run ()
