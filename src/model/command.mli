
type user = string
type mode = string
type mask = string
type service = string
type server = string
type keyed_channel = Channel.t * Channel.key option

type t =
  (* These commands are taken from RFC 2812 *)

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
  | Notice of string * string

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

  (* 5.1 Command responses *)
  | Rpl of Reply.t

  (* 5.2 Error replies *)
  | Err of Error.t

val pp_print : Format.formatter -> t -> unit

val from_strings : string -> string list -> t
(** Parse the given command and parameters as a command. May raise
   [Error.Exception]. *)
