open Irc_model
open Error

let from_error ?prefix error =
  Message.make ?prefix ~suffix:(Error_ error) ()

let nosuchnick ?prefix nick =
  from_error ?prefix (NoSuchNick nick)

let nicknameinuse ?prefix nick =
  from_error ?prefix (NicknameInUse nick)
