open Message
open Suffix
open Command
open Reply
open Error

class virtual handler = object (self)
  method virtual on_pass : Prefix.t option -> string -> unit
  method virtual on_join : Prefix.t option -> (Channel.t * Channel.key option) list -> unit
  method virtual on_privmsg : Prefix.t option -> Target.t -> string -> unit
  method virtual on_notice : Prefix.t option -> Target.t -> string -> unit

  method on_command prefix = function
    | Pass password -> self#on_pass prefix password
    | Join chans -> self#on_join prefix chans
    | Privmsg (target, content) -> self#on_privmsg prefix target content
    | Notice (target, content) -> self#on_notice prefix target content
    | _ -> assert false

  method virtual on_welcome : Prefix.t option -> Nickname.t -> string -> unit
  method virtual on_yourhost : Prefix.t option -> Nickname.t -> string -> unit
  method virtual on_created : Prefix.t option -> Nickname.t -> string -> unit
  method virtual on_myinfo : Prefix.t option -> Nickname.t -> string -> string -> string -> string -> unit
  method virtual on_bounce : Prefix.t option -> Nickname.t -> string -> unit
  method virtual on_motdstart : Prefix.t option -> Nickname.t -> string -> unit
  method virtual on_motd : Prefix.t option -> Nickname.t -> string -> unit
  method virtual on_endofmotd : Prefix.t option -> Nickname.t -> string -> unit

  method on_reply prefix = function
    | Welcome (nick, text) -> self#on_welcome prefix nick text
    | Yourhost (nick, text) -> self#on_yourhost prefix nick text
    | Created (nick, text) -> self#on_created prefix nick text
    | Myinfo (nick, servername, version, usermodes, channelmodes) -> self#on_myinfo prefix nick servername version usermodes channelmodes
    | Bounce (nick, text) -> self#on_bounce prefix nick text
    | MotdStart (nick, text) -> self#on_motdstart prefix nick text
    | Motd (nick, text) -> self#on_motd prefix nick text
    | EndOfMotd (nick, text) -> self#on_endofmotd prefix nick text
    | _ -> assert false

  method virtual on_nosuchnick : Prefix.t option -> Nickname.t -> unit

  method on_error (prefix : Prefix.t option) = function
    | NoSuchNick n -> self#on_nosuchnick prefix n
    | _ -> assert false

  method on_message message =
    match message.suffix with
    | Command command -> self#on_command message.prefix command
    | Reply reply -> self#on_reply message.prefix reply
    | Error error -> self#on_error message.prefix error
end

(* Smart Constructors *)

let pass ?prefix password =
  make ?prefix ~suffix:(Command (Pass password)) ()

let nick ?prefix nick =
  make ?prefix ~suffix:(Command (Nick nick)) ()

let user ?prefix user mode realname =
  make ?prefix ~suffix:(Command (User (user, mode, realname))) ()

let oper ?prefix name password =
  make ?prefix ~suffix:(Command (Oper (name, password))) ()

let umode ?prefix nick modes =
  make ?prefix ~suffix:(Command (Umode (nick, modes))) ()

(* let service *)

let quit ?prefix reason =
  make ?prefix ~suffix:(Command (Quit reason)) ()

let squit ?prefix server comment =
  make ?prefix ~suffix:(Command (Squit (server, comment))) ()

let join ?prefix chans =
  make ?prefix ~suffix:(Command (Join chans)) ()

let join0 ?prefix () =
  make ?prefix ~suffix:(Command Join0) ()

let part ?prefix chans reason =
  make ?prefix ~suffix:(Command (Part (chans, reason))) ()

let cmode ?prefix chan modes =
  make ?prefix ~suffix:(Command (Cmode (chan, modes))) ()

let topic ?prefix chan topic =
  make ?prefix ~suffix:(Command (Topic (chan, topic))) ()

let names ?prefix chans target =
  make ?prefix ~suffix:(Command (Names (chans, target))) ()

let list ?prefix chans target =
  make ?prefix ~suffix:(Command (List (chans, target))) ()

let invite ?prefix chan nick =
  make ?prefix ~suffix:(Command (Invite (chan, nick))) ()

let kick ?prefix chans users comment =
  make ?prefix ~suffix:(Command (Kick (chans, users, comment))) ()

let privmsg ?prefix target content =
  make ?prefix ~suffix:(Command (Privmsg (target, content))) ()

let notice ?prefix target content =
  make ?prefix ~suffix:(Command (Notice (target, content))) ()

let kill ?prefix nick comment =
  make ?prefix ~suffix:(Command (Kill (nick, comment))) ()

let ping ?prefix server1 server2 =
  make ?prefix ~suffix:(Command (Ping (server1, server2))) ()

let pong ?prefix server1 server2 =
  make ?prefix ~suffix:(Command (Pong (server1, server2))) ()

let error ?prefix message =
  make ?prefix ~suffix:(Command (Error message)) ()
