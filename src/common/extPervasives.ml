let (>>=) = Lwt.bind

let unwrap = function
  | Some x -> x
  | None -> failwith "unwrap"
