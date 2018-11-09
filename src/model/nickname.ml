
type t = string

let pp_print ppf nick =
  Format.fprintf ppf "%s" nick

let to_string nick =
  nick

let from_string nick =
  (* nickname   =  ( letter / special ) *8( letter / digit / special / "-" ) *)
  (* letter     =  %x41-5A / %x61-7A       ; A-Z / a-z *)
  (* digit      =  %x30-39                 ; 0-9 *)
  (* special    =  %x5B-60 / %x7B-7D *)
  (*                   ; "[", "]", "\\", "`", "_", "^", "{", "|", "}" *)
  if String.length nick = 0 then
    raise (Invalid_argument "Nickname.of_string");
  let c = Char.code nick.[0] in
  if not (c = 45 || (65 <= c && c <= 125)) then
    raise (Invalid_argument "Nickname.of_string");
  for i = 1 to String.length nick - 1 do
    let c = Char.code nick.[i] in
    if not (c = 45 || (48 <= c && c <= 57) || (65 <= c && c <= 125)) then
      raise (Invalid_argument "Nickname.of_string");
  done;
nick
