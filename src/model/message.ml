let fpf = Format.fprintf

(* =============================== [ Prefix ] =============================== *)

type prefix =
  | Servername of string
  | Identity of Identity.t
[@@deriving show]

let pp_print_prefix_option ppf = function
  | None -> ()
  | Some (Servername s) ->
     fpf ppf ":%s" s
  | Some (Identity id) ->
     fpf ppf ":%a" Identity.pp_print id

(* ============================== [ Message ] =============================== *)

type t =
  { prefix : prefix option ;
    suffix : Suffix.t }
[@@deriving show]

let make ?prefix ~suffix () = { prefix ; suffix }

let from_command command = make ~suffix:(Command command) ()

let prefix message = message.prefix
let suffix message = message.suffix

let pp_print fmt message =
  pp_print_prefix_option fmt message.prefix;
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
         (try Identity (Identity.from_string string)
          with Invalid_argument _ -> Servername string)
    | _ ->
       None
  in

  { prefix = prefix ;
    suffix = Suffix.from_neglexbuf lb }

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
