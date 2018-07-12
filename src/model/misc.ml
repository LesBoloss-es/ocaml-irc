
(* Because of IRC's Scandinavian origin, the characters {}|^ are
   considered to be the lower case equivalents of the characters []\~,
   respectively. This is a critical issue when determining the
   equivalence of two nicknames or channel names. *)
let lowercase_bytes b =
  let b = Bytes.lowercase_ascii b in
  Bytes.iteri
    (fun i -> function
      | '[' -> Bytes.set b i '{'
      | ']' -> Bytes.set b i '}'
      | '\\' -> Bytes.set b i '|'
      | '~' -> Bytes.set b i '^'
      | _ -> ())
    b;
  b

let lowercase s =
  Bytes.unsafe_to_string (lowercase_bytes (Bytes.of_string s))
