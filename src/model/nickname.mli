
type t
(** An abstract type for nicknames *)
   
val pp_print : Format.formatter -> t -> unit
val to_string : t -> string

val of_string : string -> t
(** Use the given string as a nickname. Raises [Error.(Exception
   ErroneousNickname)] if the string does not represent a valid
   nickname. *)
