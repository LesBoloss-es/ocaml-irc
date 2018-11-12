open Irc_common

let fpf = Format.fprintf

type t =
  { prefix : Prefix.t option ;
    suffix : Suffix.t }
[@@deriving show {with_path=false}]

let make ?prefix ~suffix () = { prefix ; suffix }

let from_command command = make ~suffix:(Command command) ()

let prefix message = message.prefix
let suffix message = message.suffix

let pp_print fmt message =
  Prefix.pp_print_option fmt message.prefix;
  Format.pp_print_string fmt (Suffix.to_string message.suffix)

let pp_print_endline fmt m =
  pp_print fmt m;
  fpf fmt "/r/n"

let to_string m =
  let buf = Buffer.create 8 in
  let ppf = Format.formatter_of_buffer buf in
  pp_print ppf m;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let to_string_endline m =
  let buf = Buffer.create 8 in
  let ppf = Format.formatter_of_buffer buf in
  pp_print_endline ppf m;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let from_string str =
  let open Result in
  let lb = NegLexing.from_string str in
  let prefix = Prefix.from_neglexbuf lb in
  Suffix.from_neglexbuf lb >>= fun suffix ->
  Ok { prefix ; suffix }
