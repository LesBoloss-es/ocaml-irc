type t

val nick : t -> Nickname.t
val nick_opt : t -> Nickname.t option

val user : t -> string
val user_opt : t -> string option

val host : t -> string
val host_opt : t -> string option

val make_opt : Nickname.t option -> string option -> string option -> t
val make : Nickname.t -> string -> string -> t

val set_nick : t -> Nickname.t -> t
val set_user : t -> string -> t

val is_valid : t -> bool

val pp_print : Format.formatter -> t -> unit
val to_string : t -> string

val from_string : string -> t

(** {2 Representation} *)

val pp : Format.formatter -> t -> unit
val show : t -> string
