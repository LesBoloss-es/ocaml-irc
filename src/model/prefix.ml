let fpf = Format.fprintf

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
