
let fpf = Format.fprintf

module Prefix =
  struct
    type prefix =
      | Servername of string
      | Identity of Identity.t

    type t = prefix option

    let pp_print ppf = function
      | None -> ()
      | Some (Servername s) ->
         fpf ppf ":%s" s
      | Some (Identity id) ->
         fpf ppf ":%a" Identity.pp_print id
  end

type t =
  { prefix : Prefix.t ;
    command : Command.t }

let make prefix command =
  { prefix = Some prefix ; command }
let make_noprefix command =
  { prefix = None ; command }

let pp_print fmt m =
  (
    match m.prefix with
    | None -> fpf fmt "%a"
    | Some (Prefix.Identity id) when not (Identity.is_valid id) -> raise (Invalid_argument "Message.pp_print")
    | Some p -> fpf fmt "%a %a" Prefix.pp_print m.prefix
  )
    Command.pp_print m.command

let pp_print_endline fmt m =
  pp_print fmt m;
  fpf fmt "/r/n"

let to_string m =
  let buf = Buffer.create 8 in
  let ppf = Format.formatter_of_buffer buf in
  pp_print ppf m;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let from_string str =
  let lb = NegLexing.of_string str in

  (* get prefix if there is one *)
  let prefix =
    match NegLexing.peek_char lb with
    | ':' ->
       NegLexing.next_char lb;
       (
         try Some (Prefix.Identity (Identity.from_string (NegLexing.next_sep ' ' lb)))
         with Not_found -> raise (Invalid_argument "Message.from_string: found a prefix but no command")
       )
    | _ ->
       None
  in

  (* get command; there must be one *)
  let command =
    try NegLexing.next_sep ' ' lb
    with Not_found -> NegLexing.remaining lb
  in

  (* get parameters *)
  let params =
    let rec find_params acc =
      try
        (
          match NegLexing.peek_char lb with
          | ':' ->
             (* trailing: we take everything from here until the end and
                check that we end indeed with a newline *)
             NegLexing.next_char lb;
             let trailing = NegLexing.remaining lb in
             trailing :: acc

          | _ ->
             (
               try
                 (* a regular parameter *)
                 find_params ((NegLexing.next_sep ' ' lb) :: acc)
               with
                 Not_found ->
                 (* the last parameter *)
                 let trailing = NegLexing.remaining lb in
                 trailing :: acc
             )
        )
      with
      | NegLexing.Error "end of lexbuf" ->
         acc
    in
    List.rev (find_params [])
  in

  { prefix = prefix ;
    command = Command.from_strings command params }
