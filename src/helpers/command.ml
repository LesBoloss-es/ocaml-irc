open Irc_model
open Command

let from_command ?prefix command =
  Message.make ?prefix ~suffix:(Command command) ()

let pass ?prefix password =
  from_command ?prefix (Pass password)

let nick ?prefix nick =
  from_command ?prefix (Nick nick)

let user ?prefix user mode realname =
  from_command ?prefix (User (user, mode, realname))

let oper ?prefix name password =
  from_command ?prefix (Oper (name, password))

let umode ?prefix nick modes =
  from_command ?prefix (Umode (nick, modes))

(* let service *)

let quit ?prefix reason =
  from_command ?prefix (Quit reason)

let squit ?prefix server comment =
  from_command ?prefix (Squit (server, comment))

let join ?prefix chans =
  from_command ?prefix (Join chans)

let join0 ?prefix () =
  from_command ?prefix Join0

let part ?prefix chans reason =
  from_command ?prefix (Part (chans, reason))

let cmode ?prefix chan modes =
  from_command ?prefix (Cmode (chan, modes))

let topic ?prefix chan topic =
  from_command ?prefix (Topic (chan, topic))

let names ?prefix chans target =
  from_command ?prefix (Names (chans, target))

let list ?prefix chans target =
  from_command ?prefix (List (chans, target))

let invite ?prefix chan nick =
  from_command ?prefix (Invite (chan, nick))

let kick ?prefix chans users comment =
  from_command ?prefix (Kick (chans, users, comment))

let privmsg ?prefix target content =
  from_command ?prefix (Privmsg (target, content))

let notice ?prefix target content =
  from_command ?prefix (Notice (target, content))

let kill ?prefix nick comment =
  from_command ?prefix (Kill (nick, comment))

let ping ?prefix server1 server2 =
  from_command ?prefix (Ping (server1, server2))

let pong ?prefix server1 server2 =
  from_command ?prefix (Pong (server1, server2))

let error ?prefix message =
  from_command ?prefix (Error message)
