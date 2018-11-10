open Irc_common

type t =
  | All
  | Channel of Channel.t
  | Nickname of Nickname.t
[@@deriving show]

let pp_print fmt = function
  | All -> Format.pp_print_string fmt "*"
  | Channel c -> Channel.pp_print fmt c
  | Nickname n -> Nickname.pp_print fmt n

let to_string = Format.to_string_of_pp_print pp_print

let from_string s =
  if s = "*" then
    All
  else
    try Nickname (Nickname.from_string s)
    with Failure _ -> Channel (Channel.from_string s)
