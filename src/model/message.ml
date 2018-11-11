let fpf = Format.fprintf

type t =
  { prefix : Prefix.t option ;
    suffix : Suffix.t }
[@@deriving show]

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
  let lb = NegLexing.from_string str in

  (* get prefix if there is one *)
  let prefix =
    match NegLexing.peek_char lb with
    | ':' ->
       NegLexing.next_char lb;
       let string = NegLexing.next_sep ' ' lb in
       Some
         (try Prefix.Identity (Identity.from_string string)
          with Invalid_argument _ -> Servername string)
    | _ ->
       None
  in

  { prefix = prefix ;
    suffix = Suffix.from_neglexbuf lb }
