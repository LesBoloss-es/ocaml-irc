
type keyed_channel = Channel.t * Channel.key option

type mask = string
let mask_to_string s = s
let mask_from_string s = s

type mode = string
let mode_to_string s = s
let mode_from_string s = s

type server = string
let server_to_string s = s
let server_from_string s = s

type service = string
let service_to_string s = s
let service_from_string s = s

type user = string
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
              [@to_strings fun _ -> []]
  | Join0
  | Part of Channel.t list * string
              [@to_strings fun _ _ -> []]
  | Mode of string * string list
              [@to_strings fun _ _ -> []]
  | Topic of string * string option
              [@to_strings fun _ _ -> []]
  | Names of string option * string option
              [@to_strings fun _ _ -> []]
  | List of string option * string option
              [@to_strings fun _ _ -> []]
  | Invite of string * string
  | Kick of string * string * string option
              [@to_strings fun _ _ _ -> []]

  (* 3.3 Sending messages *)
  | Privmsg of Target.t * string
  | Notice of string * string

  (* 3.4 Server queries and commands *)
  | Motd of string option
              [@to_strings fun _ -> []]
  | Lusers of string option * string option
              [@to_strings fun _ _ -> []]
  | Version of string option
              [@to_strings fun _ -> []]
  | Stats of string option * string option
              [@to_strings fun _ _ -> []]
  | Links of string option * string option
              [@to_strings fun _ _ -> []]
  | Time of string option
              [@to_strings fun _ -> []]
  | Connect of string * int * string option
              [@to_strings fun _ _ _ -> []]
  | Trace of string option
              [@to_strings fun _ -> []]
  | Admin of string option
              [@to_strings fun _ -> []]
  | Info of string option
              [@to_strings fun _ -> []]

  (* 3.5 Squery *)
  | Servlist of mask option * string option
              [@to_strings fun _ _ -> []]
  | Squery of service * string

  (* 3.6 Who query *)
  | Who of mask * bool
              [@to_strings fun _ _ -> []]
  | Whois of mask list * server option
              [@to_strings fun _ _ -> []]
  | Whowas of Nickname.t * int option * string option
              [@to_strings fun _ _ _ -> []]

  (* 3.7 Miscellaneous messages *)
  | Kill of string * string
  | Ping of server * server option
              [@to_strings fun _ _ -> []]
  | Pong of server * server option
              [@to_strings fun _ _ -> []]
  | Error of string

[@@deriving irc_internal_ppx]
