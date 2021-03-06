open Irc_common

type t =
  | Command of Command.t
  | Reply of Reply.t
  | Error_ of Error.t
[@@deriving show {with_path=false}]

let from_neglexbuf lb =
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
  (* parse *)
  Result.(
    first_success
      [ (fun () -> Command.from_low command params >>= fun command -> Ok (Command command)) ;
        (fun () -> Reply.from_low command params >>= fun reply -> Ok (Reply reply)) ;
        (fun () -> Error.from_low command params >>= fun error -> Ok (Error_ error)) ]
  )

let to_string suffix =
  let rec arguments_to_string = function
    | [] -> ""
    | [e] -> " :" ^ e
    | h :: t ->
       assert (not (String.contains h ' '));
       " " ^ h ^ arguments_to_string t
  in
  let (command, arguments) =
    match suffix with
    | Command command -> Command.to_low command
    | Reply reply -> Reply.to_low reply
    | Error_ error -> Error.to_low error
  in
  command ^ arguments_to_string arguments
