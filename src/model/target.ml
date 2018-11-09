open Irc_common

type t =
  | Channel of Channel.t
  | Nickname of Nickname.t

let pp_print ppf = function
  | Channel c -> Channel.pp_print ppf c
  | Nickname n -> Nickname.pp_print ppf n

let to_string = Format.to_string_of_pp_print pp_print
