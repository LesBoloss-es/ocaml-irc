open Irc_model
open Message
open Suffix
open Command
open Reply
open Error

type conn = Connection.t
type prefix = Prefix.t option

class virtual handler = object (self)
  method virtual on_pass : conn -> prefix -> string -> unit
  method virtual on_join : conn -> prefix -> (Channel.t * Channel.key option) list -> unit
  method virtual on_privmsg : conn -> prefix -> Target.t -> string -> unit
  method virtual on_notice : conn -> prefix -> Target.t -> string -> unit
  method virtual on_ping : conn -> prefix -> string -> string option -> unit
  method virtual on_pong : conn -> prefix -> string -> string option -> unit

  method on_command conn prefix = function
    | Pass password -> self#on_pass conn prefix password
    | Join chans -> self#on_join conn prefix chans
    | Privmsg (target, content) -> self#on_privmsg conn prefix target content
    | Notice (target, content) -> self#on_notice conn prefix target content
    | Ping (server1, server2) -> self#on_ping conn prefix server1 server2
    | _ -> assert false

  method virtual on_welcome : conn -> prefix -> Nickname.t -> string -> unit
  method virtual on_yourhost : conn -> prefix -> Nickname.t -> string -> unit
  method virtual on_created : conn -> prefix -> Nickname.t -> string -> unit
  method virtual on_myinfo : conn -> prefix -> Nickname.t -> string -> string -> string -> string -> unit
  method virtual on_bounce : conn -> prefix -> Nickname.t -> string -> unit
  method virtual on_motdstart : conn -> prefix -> Nickname.t -> string -> unit
  method virtual on_motd : conn -> prefix -> Nickname.t -> string -> unit
  method virtual on_endofmotd : conn -> prefix -> Nickname.t -> string -> unit

  method on_reply conn prefix = function
    | Welcome (nick, text) -> self#on_welcome conn prefix nick text
    | Yourhost (nick, text) -> self#on_yourhost conn prefix nick text
    | Created (nick, text) -> self#on_created conn prefix nick text
    | Myinfo (nick, servername, version, usermodes, channelmodes) -> self#on_myinfo conn prefix nick servername version usermodes channelmodes
    | Bounce (nick, text) -> self#on_bounce conn prefix nick text
    | MotdStart (nick, text) -> self#on_motdstart conn prefix nick text
    | Motd (nick, text) -> self#on_motd conn prefix nick text
    | EndOfMotd (nick, text) -> self#on_endofmotd conn prefix nick text
    | _ -> assert false

  method virtual on_nosuchnick : conn -> prefix -> Nickname.t -> unit

  method on_error conn prefix = function
    | NoSuchNick n -> self#on_nosuchnick conn prefix n
    | _ -> assert false

  method on_message conn message =
    match message.suffix with
    | Command command -> self#on_command conn message.prefix command
    | Reply reply -> self#on_reply conn message.prefix reply
    | Error error -> self#on_error conn message.prefix error
end
