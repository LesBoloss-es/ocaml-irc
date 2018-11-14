open Irc_model
open Reply

let from_reply ?prefix reply =
  Message.make ?prefix ~suffix:(Reply reply) ()

let welcome ?prefix ?text nick =
  let text =
    match text with
    | None -> Format.sprintf "Welcome to the Internet Relay Network, %s!" (Nickname.to_string nick)
    | Some text -> text
  in
  from_reply ?prefix (Welcome (nick, text))

let yourhost ?prefix ?(text="Your host is %%NAME%% running version %%VERSION%%") nick =
  from_reply ?prefix (YourHost (nick, text))

let created ?prefix ?(text="This server was created some time ago") nick =
  from_reply ?prefix (Created (nick, text))

let myinfo ?prefix nick servername version user_modes channel_modes =
  from_reply ?prefix (MyInfo (nick, servername, version, user_modes, channel_modes))
