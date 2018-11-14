open Irc_model
open Error

let from_error ?prefix error =
  Message.make ?prefix ~suffix:(Error_ error) ()

let nosuchnick ?prefix nick =
  from_error ?prefix (NoSuchNick nick)

let nicknameinuse ?prefix ?(text="Nickname is already in use") nick =
  from_error ?prefix (NicknameInUse (nick, text))

let badchannelkey ?prefix ?(text="Cannot join channel (+k)") chan =
  from_error ?prefix (BadChannelKey (chan, text))
