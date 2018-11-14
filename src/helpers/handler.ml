open Irc_model
open Message
open Suffix
open Command
open Reply
open Error

type conn = Connection.t
type prefix = Prefix.t option

module type Events = sig
  val on_connection : conn -> unit

  val on_pass : conn -> prefix -> string -> unit
  val on_nick : conn -> prefix -> Nickname.t -> unit
  val on_user : conn -> prefix -> user -> mode -> string -> unit

  val on_join : conn -> prefix -> (Channel.t * Channel.key option) list -> unit
  val on_join0 : conn -> prefix -> unit
  val on_part : conn -> prefix -> Channel.t list -> string -> unit
  val on_privmsg : conn -> prefix -> Target.t -> string -> unit
  val on_notice : conn -> prefix -> Target.t -> string -> unit
  val on_ping : conn -> prefix -> string -> string option -> unit
  val on_pong : conn -> prefix -> string -> string option -> unit
  val on_quit : conn -> prefix -> string -> unit

  val on_welcome : conn -> prefix -> Nickname.t -> string -> unit
  val on_yourhost : conn -> prefix -> Nickname.t -> string -> unit
  val on_created : conn -> prefix -> Nickname.t -> string -> unit
  val on_myinfo : conn -> prefix -> Nickname.t -> string -> string -> string -> string -> unit
  val on_bounce : conn -> prefix -> Nickname.t -> string -> unit
  val on_motdstart : conn -> prefix -> Nickname.t -> string -> unit
  val on_motd : conn -> prefix -> Nickname.t -> string -> unit
  val on_endofmotd : conn -> prefix -> Nickname.t -> string -> unit

  val on_nosuchnick : conn -> prefix -> Nickname.t -> unit
end

module type Handler = sig
  val on_connection : conn -> unit
  val on_message : conn -> Message.t -> unit
end

module Make (E : Events) : Handler = struct
  let on_connection conn = E.on_connection conn

  let on_command conn prefix = function
    | Pass password -> E.on_pass conn prefix password
    | Nick nick -> E.on_nick conn prefix nick
    | User (user, mode, realname) -> E.on_user conn prefix user mode realname
    | Join chans -> E.on_join conn prefix chans
    | Join0 -> E.on_join0 conn prefix
    | Part (chans, reason) -> E.on_part conn prefix chans reason
    | Privmsg (target, content) -> E.on_privmsg conn prefix target content
    | Notice (target, content) -> E.on_notice conn prefix target content
    | Ping (server1, server2) -> E.on_ping conn prefix server1 server2
    | Quit reason -> E.on_quit conn prefix reason
    | _ -> assert false

  let on_reply conn prefix = function
    | Welcome (nick, text) -> E.on_welcome conn prefix nick text
    | YourHost (nick, text) -> E.on_yourhost conn prefix nick text
    | Created (nick, text) -> E.on_created conn prefix nick text
    | MyInfo (nick, servername, version, usermodes, channelmodes) -> E.on_myinfo conn prefix nick servername version usermodes channelmodes
    | Bounce (nick, text) -> E.on_bounce conn prefix nick text
    | MotdStart (nick, text) -> E.on_motdstart conn prefix nick text
    | Motd (nick, text) -> E.on_motd conn prefix nick text
    | EndOfMotd (nick, text) -> E.on_endofmotd conn prefix nick text
    | _ -> assert false

  let on_error conn prefix = function
    | NoSuchNick n -> E.on_nosuchnick conn prefix n
    | _ -> assert false

  let on_message conn message =
    match message.suffix with
    | Command command -> on_command conn message.prefix command
    | Reply reply -> on_reply conn message.prefix reply
    | Error_ error -> on_error conn message.prefix error
end
