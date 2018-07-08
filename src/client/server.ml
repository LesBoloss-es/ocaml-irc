open Irc_utils
open Irc_model
let (>>=) = Lwt.bind

type t =
  { address : string ;
    port : int ;
    socket : Socket.t }

let make address port =
  { address ; port ; socket = Socket.make address port }
  
let connect server =
  Socket.connect server.socket
  
let send server message : unit Lwt.t =
  Message.to_string_endline message
  |> Socket.output server.socket
