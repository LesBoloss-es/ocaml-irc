type ('a, 'b) t = ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

let show show_ok = function
  | Ok a -> show_ok a
  | Error _ -> "<error>"

let (>>=) v f =
  match v with
  | Ok v -> f v
  | Error e -> Error e

let rec first_success ?(ifnot=()) = function
  | [] -> Error ifnot
  | h :: t ->
     match h () with
     | Ok v -> Ok v
     | Error _ -> first_success t
