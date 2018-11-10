open Irc_common

type keyed_channel = Channel.t * Channel.key option
[@@deriving show]

type mask = string [@@deriving show]
let mask_to_string s = s
let mask_from_string s = s

type mode = int [@@deriving show]
let mode_to_string = string_of_int
let mode_from_string = int_of_string

type server = string [@@deriving show]
let server_to_string s = s
let server_from_string s = s

type service = string [@@deriving show]
let service_to_string s = s
let service_from_string s = s

type user = string [@@deriving show]
let user_to_string s = s
let user_from_string s = s

type t =
  (* 3.1 Connection Registration *)
  | Pass of string
  | Nick of Nickname.t
  | User of user * mode * string
  | Oper of string * string
  (* | Mode of string * string list *) (*FIXME: chan vs. user modes*)
  | Service of string * string * string * string * string * string
  | Quit of string
  | Squit of string * string

  (* 3.2 Channel operations *)
  | Join of keyed_channel list
  | Join0
  | Part of Channel.t list * string
  | Mode of string * string list
  | Topic of string * string option
  | Names of string option * string option
  | List of string option * string option
  | Invite of string * string
  | Kick of string * string * string option

  (* 3.3 Sending messages *)
  | Privmsg of Target.t * string
  | Notice of Target.t * string

  (* 3.4 Server queries and commands *)
  | Motd of string option
  | Lusers of string option * string option
  | Version of string option
  | Stats of string option * string option
  | Links of string option * string option
  | Time of string option
  | Connect of string * int * string option
  | Trace of string option
  | Admin of string option
  | Info of string option

  (* 3.5 Squery *)
  | Servlist of mask option * string option
  | Squery of service * string

  (* 3.6 Who query *)
  | Who of mask * bool
  | Whois of mask list * server option
  | Whowas of Nickname.t * int option * string option

  (* 3.7 Miscellaneous messages *)
  | Kill of string * string
  | Ping of server * server option
  | Pong of server * server option
  | Error of string
[@@deriving show]

let from_low command arguments =
  match command, arguments with
  | "PASS",   [password]     -> Pass password
  | "NICK",   [nick]         -> Nick (Nickname.from_string nick)
  | "NOTICE", [target; text] -> Notice (Target.from_string target, text)
  | _ -> assert false

let to_low = function
  | Pass password               -> "PASS",    [password]
  | Nick nick                   -> "NICK",    [Nickname.to_string nick]
  | User (user, mode, realname) -> "USER",    [user; mode_to_string mode; "*"; realname]
  | Privmsg (target, content)   -> "PRIVMSG", [Target.to_string target; content]
  | Notice (target, content)    -> "NOTICE",  [Target.to_string target; content]
  | Join channels               ->
     (
       let channels, keys =
         channels
         |> List.sort
              (fun (_, key1) (_, key2) ->
                match key1, key2 with
                | Some _, None -> -1
                | None, Some _ -> 1
                | _ -> 0)
         |> List.split
       in
       let channels =
           channels
           |> List.map Channel.to_string
           |> String.concat ","
       in
       let keys =
         keys
         |> List.filter ((<>) None)
         |> List.map unwrap
         |> List.map Channel.key_to_string
         |> String.concat ","
       in
       "JOIN", [channels; keys]
     )
  | _ -> assert false

class virtual ['prefix] handler = object (self)
  method virtual on_pass : 'prefix -> string -> unit
  method virtual on_privmsg : 'prefix -> Target.t -> string -> unit
  method virtual on_notice : 'prefix -> Target.t -> string -> unit

  method on_command prefix = function
    | Pass password -> self#on_pass prefix password
    | Privmsg (target, content) -> self#on_privmsg prefix target content
    | Notice (target, content) -> self#on_notice prefix target content
    | _ -> assert false
end
