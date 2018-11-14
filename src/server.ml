open Irc_common
module Conn = Irc_helpers.Connection

module type Config = sig
  val listen_address : string
  val listen_port : int
  val servername : string
end

module type S = sig
  val on_connection : Conn.t -> unit Lwt.t
  val build : unit -> Lwt_io.server Lwt.t
  val run_async : unit -> Lwt_io.server
end

module FromHandler (C : Config) (H : Irc_helpers.Handler.Handler) : S = struct

  let on_connection conn =
    H.on_connection conn;
    Conn.receive_stream conn
    |> Lwt_stream.iter
         (function
          | Ok message -> H.on_message conn message
          | Error () -> ()) (* FIXME: better than that *)

  let build () =
    let listen_address =
      match C.listen_address with
      | "any" -> Unix.inet_addr_any
      | _ -> Unix.inet_addr_of_string C.listen_address
    in
    let my_sockaddr = Unix.ADDR_INET (listen_address, C.listen_port) in
    Lwt_io.establish_server_with_client_socket
      my_sockaddr
      (fun _ socket ->
        let conn = Conn.make ~socket in
        on_connection conn)

  let run_async () : Lwt_io.server =
    Lwt_main.run (build ())
end

module FromEvents (C: Config) (E : Irc_helpers.Handler.Events) : S = FromHandler (C) (Irc_helpers.Handler.Make(E))

open Irc_model
open Irc_helpers.Command
open Irc_helpers.Reply
open Irc_helpers.Error

type client_data =
  { mutable identity : Identity.t ;
    mutable realname : string option ;
    mutable welcomed : bool ;
    mutable channels : Channel.t list }

type channel_data =
  { mutable key : Channel.key option }

module GenericEvents (C : Config) : Irc_helpers.Handler.Events = struct
  let clients = Conn.Table.create 8
  let channels = Hashtbl.create 8

  let with_client_data client fun_ =
    Conn.Table.with_ clients client fun_ (fun () -> ()) (* FIXME *)

  let send_to_chan ?except channel message =
    Conn.Table.to_list clients
    |> List.filter
         (fun (client, data) ->
           (match except with None -> true | Some client' -> not (Conn.equal client client'))
           && List.mem channel data.channels)
    |> List.iter
         (fun (client, _) ->
           Conn.send_async client message)

  let on_connection client =
    Conn.Table.add clients client
      { identity = Identity.(set_host empty (Conn.host client)) ;
        realname = None ;
        welcomed = false ;
        channels = [] }

  let on_welcome _ _ _ _ = ()
  let on_yourhost _ _ _ _ = ()
  let on_created _ _ _ _ = ()
  let on_myinfo _ _ _ _ _ _ _ = ()
  let on_bounce _ _ _ _ = ()

  let on_motdstart _ _ _ _ = ()
  let on_motd _ _ _ _ = ()
  let on_endofmotd _ _ _ _ = ()

  let on_pass _ _ _ = ()

  let on_join client _ chans =
    with_client_data client @@ fun data ->
    chans
    |> List.iter
         (fun (chan, key) ->
           let cdata =
             try Hashtbl.find channels chan
             with Not_found ->
               let cdata = { key = None } in
               Hashtbl.add channels chan cdata;
               cdata
           in
           if key = cdata.key then
             if not (List.mem chan data.channels) then
               (
                 data.channels <- chan :: data.channels;
                 send_to_chan chan (join ~prefix:(Identity data.identity) [chan,None])
               )
             else
               ()
           else
             Conn.send_async client (badchannelkey chan))

  let on_join0 client _ =
    with_client_data client @@ fun data ->
    data.channels
    |> List.iter
         (fun chan ->
           send_to_chan chan (part ~prefix:(Identity data.identity) [chan]));
    data.channels <- []

  let on_part client _ chans reason =
    with_client_data client @@ fun data ->
    chans
    |> List.iter
         (fun chan ->
           if List.mem chan data.channels then
             send_to_chan chan (part ~prefix:(Identity data.identity) ~reason [chan]));
    data.channels <- List.filter (fun chan -> not (List.mem chan data.channels)) data.channels

  let on_notice _ _ _ _ = ()

  let on_ping conn _ server1 server2 =
    let prefix = Prefix.Servername C.servername in
    Conn.send_async conn (pong ~prefix server1 server2)

  let on_pong _ _ _ _ = ()

  let on_nosuchnick _ _ _ = ()

  let welcome_if_complete client =
    with_client_data client @@ fun data ->
    if not data.welcomed && Identity.is_complete data.identity then
      (
        let nick = Identity.nick data.identity in
        let prefix = Prefix.Servername C.servername in
        Conn.send_async_l client [
            welcome ~prefix nick;
            yourhost ~prefix nick;
            created ~prefix nick;
            myinfo ~prefix nick "" "" "" ""];
        data.welcomed <- true
      )

  let on_user client _ username _ realname =
    with_client_data client @@ fun data ->
    data.identity <- Identity.set_user data.identity username;
    data.realname <- Some realname;
    welcome_if_complete client

  let on_nick client _ nick =
    with_client_data client @@ fun data ->
    let nickinuse =
      Conn.Table.to_list clients
      |> List.exists
           (fun (_, data) ->
             Identity.nick_opt data.identity = Some nick)
    in
    if nickinuse then
      Conn.send_async client (nicknameinuse nick)
    else
      data.identity <- Identity.set_nick data.identity nick;
    welcome_if_complete client

  let on_privmsg client _ target content =
    with_client_data client @@ fun data ->
    let msg = privmsg ~prefix:(Identity data.identity) target content in
    match target with
    | Target.All -> () (* FIXME *)
    | Target.Channel tchan -> send_to_chan ~except:client tchan msg
    | Target.Nickname tnick ->
       (
         try
           let (tclient, _) =
             Conn.Table.to_list clients
             |> List.find
                  (fun (_, odata) ->
                    Identity.nick_opt odata.identity = Some tnick)
           in
           Conn.send_async tclient msg
         with
           Not_found -> Conn.send_async client (nosuchnick tnick)
       )

  let on_quit client _ reason =
    with_client_data client @@ fun data ->
    data.channels
    |> List.iter
         (fun chan ->
           send_to_chan ~except:client chan (quit ~prefix:(Identity data.identity) reason));
    Conn.Table.remove clients client
end

module GenericServer (C : Config) = FromEvents (C) (GenericEvents (C))
