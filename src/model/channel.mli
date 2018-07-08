
type t
type key = string

val of_string : string -> t
val to_string : t -> string
val pp_print : Format.formatter -> t -> unit
