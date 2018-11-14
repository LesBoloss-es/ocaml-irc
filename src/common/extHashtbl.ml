include Hashtbl

let with_ table key try_ with_ =
  match find_opt table key with
  | None -> with_ ()
  | Some value -> try_ value
