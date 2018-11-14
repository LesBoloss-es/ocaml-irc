open Irc_common

type t =
  { nick : Nickname.t option ;
    user : string option ;
    host : string option }
[@@deriving show]

let empty = { nick = None ; user = None ; host = None }

let nick_opt id = id.nick
let nick id = unwrap (nick_opt id)
let has_nick id = nick_opt id <> None
let set_nick id nick = { id with nick = Some nick }

let user_opt id = id.user
let user id = unwrap (user_opt id)

let host_opt id = id.host
let host id = unwrap (host_opt id)
let has_host id = host_opt id <> None
let set_host id host = { id with host = Some host }

let make_opt nick user host = { nick ; user ; host }
let make nick user host =
  { nick = Some nick ;
    user = if user = "" then None else Some user ;
    host = if host = "" then None else Some host }

let set_user id user =
  { id with user = Some user }

let is_valid id =
  id.nick <> None && not (id.user <> None && id.host = None)

let is_complete id =
  id.nick <> None && id.user <> None && id.host <> None

let pp_print ppf id =
  Nickname.pp_print ppf (nick id) ;
  if id.user <> None then Format.fprintf ppf "!~%s" (user id);
  if id.host <> None then Format.fprintf ppf "%@%s" (host id)

let to_string = Format.to_string_of_pp_print pp_print

let from_string str =
  let buf = NegLexing.from_string str in
  match NegLexing.next_sep '!' buf with
  | exception Not_found ->
     (
       match NegLexing.next_sep '@' buf with
       | exception Not_found ->
          { nick = Some (Nickname.from_string (NegLexing.remaining buf)) ;
            user = None ;
            host = None }
       | nick ->
          { nick = Some (Nickname.from_string nick) ;
            user = None ;
            host = Some (NegLexing.remaining buf) }
     )
  | nick ->
     (
       match NegLexing.next_sep '@' buf with
       | exception Not_found -> raise (Invalid_argument "Identity.from_string")
       | user ->
          { nick = Some (Nickname.from_string nick) ;
            user = Some user ;
            host = Some (NegLexing.remaining buf) }
     )
