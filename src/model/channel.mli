type t
type key = string

val from_string : string -> t
val to_string : t -> string
val pp_print : Format.formatter -> t -> unit

val key_to_string : key -> string

(** {2 Representation} *)

val pp : Format.formatter -> t -> unit
val show : t -> string

val pp_key : Format.formatter -> key -> unit
val show_key : key -> string
