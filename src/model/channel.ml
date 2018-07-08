
type t = string
type key = string

let is_valid s =
  (* FIXME: more complicated than that *)
  let l = String.length s in
  0 < l && l <= 50
  && (s.[0] = '&' || s.[0] = '#' || s.[0] = '+' || s.[0] = '!')
  && (String.index_opt s ' ' = None)
  && (String.index_opt s ',' = None)
  && (String.index_opt s (Char.chr 7) = None)

let of_string s =
  if is_valid s then
    Misc.lowercase s
  else
    raise (Invalid_argument "Channel.of_string")

let to_string s =
  s

let pp_print =
  Format.pp_print_string
