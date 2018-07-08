
type t
(** Abstract type for messages *)

val make_noprefix : Command.t -> t

val prefix : t -> Prefix.t

val command : t -> Command.t

val pp_print : Format.formatter -> t -> unit
(** Prints a message to the given formatter. *)

val pp_print_endline : Format.formatter -> t -> unit
(** Same as {!pp_print} but adds a newline. *)

val to_string : t -> string
val to_string_endline : t -> string

val from_string : string -> t
