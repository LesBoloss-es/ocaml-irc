open Irc_model

class m = object (self)
  inherit Skeleton.m as super

  method on_privmsg (server: Server.t) (source: Nickname.t) (target: Target.t) (content: string) : unit Lwt.t =
    Lwt.return ()
end
