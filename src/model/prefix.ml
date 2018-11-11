let fpf = Format.fprintf

type t =
  | Servername of string
  | Identity of Identity.t
[@@deriving show]

let pp_print_option ppf = function
  | None -> ()
  | Some (Servername s) ->
     fpf ppf ":%s" s
  | Some (Identity id) ->
     fpf ppf ":%a" Identity.pp_print id
