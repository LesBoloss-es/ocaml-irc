let fpf = Format.fprintf

(* =============================== [ Prefix ] =============================== *)

type prefix =
  | Servername of string
  | Identity of Identity.t

let pp_print_prefix_option ppf = function
  | None -> ()
  | Some (Servername s) ->
     fpf ppf ":%s" s
  | Some (Identity id) ->
     fpf ppf ":%a" Identity.pp_print id

(* =============================== [ Suffix ] =============================== *)

type suffix =
  | Command of Command.t
  | Reply of Reply.t
  | Error of Error.t

let pp_print_suffix fmt = function
  | Command command -> Command.pp_print fmt command
  | Reply reply -> Reply.pp_print fmt reply
  | Error error -> Error.pp_print fmt error

(* ============================== [ Message ] =============================== *)

type t = { prefix : prefix option ; suffix : suffix }

let make prefix suffix = { prefix = Some prefix ; suffix }
let make_noprefix suffix = { prefix = None ; suffix }

let prefix message = message.prefix
let suffix message = message.suffix

let pp_print fmt message =
  pp_print_prefix_option fmt message.prefix;
  pp_print_suffix fmt message.suffix

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
  let lb = NegLexing.of_string str in

  (* get prefix if there is one *)
  let prefix =
    match NegLexing.peek_char lb with
    | ':' ->
       NegLexing.next_char lb;
       (
         try Some (Identity (Identity.from_string (NegLexing.next_sep ' ' lb)))
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
    suffix = Command (Command.from_sl (command :: params)) }

(* ============================== [ Handler ] =============================== *)

class virtual handler = object (self)
  inherit [prefix option] Command.handler
  inherit [prefix option] Reply.handler
  inherit [prefix option] Error.handler

  method on_message message =
    match message.suffix with
    | Command command -> self#on_command message.prefix command
    | Reply reply -> self#on_reply message.prefix reply
    | Error error -> self#on_error message.prefix error
end
