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
  | Umode of Nickname.t * string list
  | Service of string * string * string * string * string * string
  | Quit of string
  | Squit of string * string

  (* 3.2 Channel operations *)
  | Join of keyed_channel list
  | Join0
  | Part of Channel.t list * string
  | Cmode of Channel.t * string list
  | Topic of Channel.t * string option
  | Names of Channel.t list * string option
  | List of Channel.t list * string option
  | Invite of Nickname.t * Channel.t
  | Kick of Channel.t list * string list * string option

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
  | Kill of Nickname.t * string
  | Ping of server * server option
  | Pong of server * server option
  | Error of string
[@@deriving show {with_path=false}]

let from_low command arguments =
  match command, arguments with
  | "PASS",    [password]     ->
     Ok (Pass password)

  | "NICK",    [nick]         ->
     Ok (Nick (Nickname.from_string nick))

  | "USER",    [user; mode; realname] ->
     Ok (User (user, mode_from_string mode, realname))

  | "PRIVMSG", [target; text] ->
     Ok (Privmsg (Target.from_string target, text))

  | "NOTICE",  [target; text] ->
     Ok (Notice (Target.from_string target, text))

  | "JOIN",    ["0"]          -> Ok Join0
  | "JOIN",    [chans]
  | "JOIN",    [chans; _]     ->
     Ok (Join [Channel.from_string chans, None]) (* FIXME !!! *)

  | "PING",    [server1]      ->
     Ok (Ping (server1, None))
  | "PING",    [server1; server2] ->
     Ok (Ping (server1, Some server2))

  | "PONG",    [server1]      ->
     Ok (Pong (server1, None))
  | "PONG",    [server1; server2] ->
     Ok (Pong (server1, Some server2))

  | "QUIT",    [message]      ->
     Ok (Quit message)
  | "ERROR",   [message]      ->
     Ok (Error message)

  | _ -> Error ()

let to_low = function
  | Pass password ->
     ("PASS", [password])

  | Nick nick ->
     ("NICK", [Nickname.to_string nick])

  | User (user, mode, realname) ->
     ("USER", [user; mode_to_string mode; "*"; realname])

  | Privmsg (target, content) ->
     ("PRIVMSG", [Target.to_string target; content])

  | Notice (target, content) ->
     ("NOTICE", [Target.to_string target; content])

  | Join channels ->
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
       ("JOIN", [channels; keys])
     )

  | Ping (server1, None) ->
     ("PING", [server1])
  | Ping (server1, Some server2) ->
     ("PING", [server1; server2])

  | Pong (server1, None) ->
     ("PONG", [server1])
  | Pong (server1, Some server2) ->
     ("PONG", [server1; server2])

  | Quit message ->
     ("QUIT", [message])

  | Error message ->
     ("ERROR", [message])

  | _ -> assert false
