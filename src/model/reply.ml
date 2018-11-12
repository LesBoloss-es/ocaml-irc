let (||>) f g x = f x |> g

type t =
  | Welcome of Nickname.t * string
  | Yourhost of Nickname.t * string
  | Created of Nickname.t * string
  | Myinfo of Nickname.t * string * string * string * string
  | Bounce of Nickname.t * string

  | TraceConnecting of string * string                                [@repr "201"]
  | TraceHandshake of string * string                                 [@repr "202"]
  | TraceUnknown of string * string                                   [@repr "203"]
  | TraceOperator of string * Nickname.t                              [@repr "204"]
  | TraceUser of string * Nickname.t                                  [@repr "205"]

  | TraceService of string * string * string * string                 [@repr "207"]
  | TraceNewType of string * string                                   [@repr "208"]
  | TraceClass of string * string                                     [@repr "209"]

  | StatlowinkInfo of string * string * string * string * string * string * string
                                                                      [@repr "211"]
  | StatsCommands of string * string * string * string                [@repr "212"]

  | EndOfStats of string                                              [@repr "219"] [@optarg "End of STATS report"]

  | UmodeIs of string                                                 [@repr "221"]

  | ServList of string * string * string * string * string * string   [@repr "234"]
              [@to_strings fun _ _ _ _ _ _ -> []]
  | ServListEnd of string * string                                    [@repr "235"] [@optarg "End of service listing"]

  | StatsUptime of int * int * int * int                              [@repr "242"]

  | LuserClient of int * int * int                                    [@repr "251"]
  | LuserOp of int                                                    [@repr "252"] [@optarg "operator/s online"]
  | LuserUnknown of int                                               [@repr "253"] [@optarg "unknown connection/s"]
  | LuserChannels of int                                              [@repr "254"] [@optarg "channel/s formed"]
  | LuserMe of int * int                                              [@repr "255"]
  | AdminMe of string                                                 [@repr "256"] [@optarg "Administrative info"]
  | AdminLoc1 of string                                               [@repr "257"]
  | AdminLoc2 of string                                               [@repr "258"]
  | AdminEmail of string                                              [@repr "259"]

  | TraceLog of string * string                                       [@repr "261"]
  | TraceEnd of string * string                                       [@repr "262"] [@optarg "End of TRACE"]
  | TryAgain of string                                                [@repr "263"] [@optarg "Please wait a while and try again."]

  | Away of Nickname.t * string                                       [@repr "301"]
  | UserHost of (Nickname.t * bool * bool * string) list              [@repr "302"]
                  [@from_strings fun _ -> UserHost []]
                  [@to_strings fun _ -> []]
  | IsOn of Nickname.t list                                           [@repr "303"]
              [@to_strings fun _ -> []]
  | UnAway                                                            [@repr "305"] [@optarg "You are no longer marked as being away"]
  | NowAway                                                           [@repr "306"] [@optarg "You have been marked as being away"]

  | WhoisUser of Nickname.t * string * string * string                [@repr "311"]
  | WhoisServer of Nickname.t * string * string                       [@repr "312"]
  | WhoisOperator of Nickname.t                                       [@repr "313"] [@optarg "is an IRC operator"]
  | WhoWasUser of Nickname.t * string * string * string               [@repr "314"]
  | EndOfWho of string                                                [@repr "315"] [@optarg "End of WHO list"]

  | WhoisIdle of Nickname.t * int                                     [@repr "317"] [@optarg "seconds idle"]
  | EndOfWhois of Nickname.t                                          [@repr "318"] [@optarg "End of WHOIS list"]

  | List of Channel.t * int * string                                  [@repr "322"]
  | ListEnd                                                           [@repr "323"] [@optarg "End of LIST"]
  | ChannelModeIs of Channel.t * string * string                      [@repr "324"]
  | UniqOpIs of Channel.t * Nickname.t                                [@repr "325"]

  | NoTopic of Channel.t                                              [@repr "331"] [@optarg "No topic is set"]
  | Topic of Channel.t * string                                       [@repr "332"]

  | EndOfNames of Channel.t                                           [@repr "336"] [@optarg "End of NAMES list"]

  | Inviting of Channel.t * Nickname.t                                [@repr "341"]
  | Summoning of string                                               [@repr "342"] [@optarg "Summoning user to IRC"]

  | InviteList of Channel.t * string                                  [@repr "346"]
  | EndOfInviteList of Channel.t                                      [@repr "347"] [@optarg "End of channel invite list"]
  | ExceptList of Channel.t * string                                  [@repr "348"]
  | EndOfExceptList of Channel.t                                      [@repr "349"] [@optarg "End of channel exception list"]

  | Version of string * string * string * string                      [@repr "351"]
  | WhoReply of Channel.t * string * string * string * Nickname.t * string * string * string * string * string
                                                                      [@repr "352"]

  | Links of string * string * string * string                        [@repr "364"]
  | EndOfLinks of string                                              [@repr "365"] [@optarg "End of LINKS list"]

  | BanList of Channel.t * string                                     [@repr "367"]
  | EndOfBanList of Channel.t                                         [@repr "368"] [@optarg "End of channel ban list"]
  | EndOfWhoWas of Nickname.t                                         [@repr "369"] [@optarg "End of WHOWAS"]

  | RInfo of string                                                   [@repr "371"]
  | Motd of Nickname.t * string

  | EndOfInfo                                                         [@repr "374"] [@optarg "End of INFO list"]
  | MotdStart of Nickname.t * string
  | EndOfMotd of Nickname.t * string

  | YoureOper                                                         [@repr "381"] [@optarg "You are now an IRC operator"]
  | Rehashing of string                                               [@repr "382"] [@optarg "Rehashing"]
  | YoureService of string                                            [@repr "383"]

  | Time of string * string                                           [@repr "391"]
  | UsersStart                                                        [@repr "392"]
  | Users of string * string * string                                 [@repr "393"]
  | EndOfUsers                                                        [@repr "394"] [@optarg "End of users"]
  | NoUsers                                                           [@repr "395"] [@optarg "Nobody logged in"]

  | WhoisChannels
  | NamReply
  | TraceLink
  | TraceServer
  | StatsOline
[@@deriving show {with_path=false}]

let from_low command arguments =
  match command, arguments with
  | "001", [nick; text] ->
     Ok (Welcome (Nickname.from_string nick, text))

  | "002", [nick; text] ->
     Ok (Yourhost (Nickname.from_string nick, text))

  | "003", [nick; text] ->
     Ok (Created (Nickname.from_string nick, text))

  | "004", [nick; servername; version; usermodes; channelmodes] ->
     Ok (Myinfo (Nickname.from_string nick, servername, version, usermodes, channelmodes))

  | "005", [nick; text] ->
     Ok (Bounce (Nickname.from_string nick, text))

  | "372", [nick; text] ->
     Ok (Motd (Nickname.from_string nick, text))

  | "375", [nick; text] ->
     Ok (MotdStart (Nickname.from_string nick, text))

  | "376", [nick; text] ->
     Ok (EndOfMotd (Nickname.from_string nick, text))

  | _ -> Error ()

let to_low = function
  | Welcome (nick, text) -> ("001", [Nickname.to_string nick; text])
  | _ -> assert false
