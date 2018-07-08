open Irc.Client
open Irc.Model

let mirror = object (self)
  inherit generic as super

  method! on_privmsg server source target content =
    let open Command in
    (
      match target with
      | Channel channel ->
         Privmsg (Channel channel, content)
      | Nickname _ ->
         Privmsg (Nickname source, content)
    )
    |> Message.make_noprefix
    |> Server.send server
end

let () = mirror#start ()
