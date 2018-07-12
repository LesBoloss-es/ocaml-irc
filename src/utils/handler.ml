class ['env] std_handler = object
  method on_bool : 'env -> bool -> unit = fun _ _ -> ()
  method on_int : 'env -> int -> unit = fun _ _ -> ()
  method on_string : 'env -> string -> unit = fun _ _ -> ()
  method on_option : 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit = fun _ _ _ -> ()
  method on_list : 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit = fun _ _ _  -> ()
end
