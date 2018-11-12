let fpf = Format.fprintf

type t =
  | Servername of string
  | Identity of Identity.t
[@@deriving show {with_path=false}]

let pp_print_option ppf = function
  | None -> ()
  | Some (Servername s) ->
     fpf ppf ":%s" s
  | Some (Identity id) ->
     fpf ppf ":%a" Identity.pp_print id

let from_neglexbuf lb =
  match NegLexing.peek_char lb with
  | ':' ->
     NegLexing.next_char lb;
     let string = NegLexing.next_sep ' ' lb in
     Some
       (try Identity (Identity.from_string string)
        with Invalid_argument _ -> Servername string)
  | _ ->
     None
