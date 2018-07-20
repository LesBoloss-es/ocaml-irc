
type t =
  | Welcome of string                                                 [@repr "001"]
  | Yourhost of string                                                [@repr "002"]
  | Created of string                                                 [@repr "003"]
  | Myinfo of Nickname.t * string * string * string * string          [@repr "004"]
  | Bounce of string                                                  [@repr "005"]

  | TraceConnecting of string * string                                [@repr "201"] (*FIXME*)
  | TraceHandshake of string * string                                 [@repr "202"] (*FIXME*)
  | TraceUnknown of string * string                                   [@repr "203"] (*FIXME*)
  | TraceOperator of string * Nickname.t                              [@repr "204"] (*FIXME*)
  | TraceUser of string * Nickname.t                                  [@repr "205"]

  | TraceService of string * string * string * string                 [@repr "207"] (*FIXME*)
  | TraceNewType of string * string                                   [@repr "208"] (*FIXME*)
  | TraceClass of string * string                                     [@repr "209"] (*FIXME*)

  | StatsLinkInfo of string * string * string * string * string * string * string
                                                                      [@repr "211"]
  | StatsCommands of string * string * string * string                [@repr "212"]

  | EndOfStats of string                                              [@repr "219"] [@optarg "End of STATS report"]

  | UmodeIs of string                                                 [@repr "221"]

  | ServList of string * string * string * string * string * string   [@repr "234"]
              [@to_strings fun _ _ _ _ _ _ -> []]
  | ServListEnd of string * string                                    [@repr "235"] [@optarg "End of service listing"]

  | StatsUptime of int * int * int * int                              [@repr "242"]

  | LuserClient of int * int * int                                    [@repr "251"] (*FIXME*)
  | LuserOp of int                                                    [@repr "252"] [@optarg "operator/s online"]
  | LuserUnknown of int                                               [@repr "253"] [@optarg "unknown connection/s"]
  | LuserChannels of int                                              [@repr "254"] [@optarg "channel/s formed"]
  | LuserMe of int * int                                              [@repr "255"] (*FIXME*)
  | AdminMe of string                                                 [@repr "256"] [@optarg "Administrative info"]
  | AdminLoc1 of string                                               [@repr "257"]
  | AdminLoc2 of string                                               [@repr "258"]
  | AdminEmail of string                                              [@repr "259"]

  | TraceLog of string * string                                       [@repr "261"] (*FIXME*)
  | TraceEnd of string * string                                       [@repr "262"] [@optarg "End of TRACE"]
  | TryAgain of string                                                [@repr "263"] [@optarg "Please wait a while and try again."]

  | Away of Nickname.t * string                                       [@repr "301"]
  | UserHost of (Nickname.t * bool * bool * string) list              [@repr "302"]
                  [@from_strings fun _ -> UserHost []] (*FIXME*)
                  [@to_strings fun _ -> []] (*FIXME*)
  | IsOn of Nickname.t list                                           [@repr "303"] (*FIXME*)
              [@to_strings fun _ -> []]
  | UnAway                                                            [@repr "305"] [@optarg "You are no longer marked as being away"]
  | NowAway                                                           [@repr "306"] [@optarg "You have been marked as being away"]

  | WhoisUser of Nickname.t * string * string * string                [@repr "311"] (*FIXME*)
  | WhoisServer of Nickname.t * string * string                       [@repr "312"]
  | WhoisOperator of Nickname.t                                       [@repr "313"] [@optarg "is an IRC operator"]
  | WhoWasUser of Nickname.t * string * string * string               [@repr "314"] (*FIXME*)
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
  | Motd of string                                                    [@repr "372"] (*FIXME*)

  | EndOfInfo                                                         [@repr "374"] [@optarg "End of INFO list"]
  | MotdStart of string                                               [@repr "375"] (*FIXME*)
  | EndOfMotd                                                         [@repr "376"] [@optarg "End of MOTD command"]

  | YoureOper                                                         [@repr "381"] [@optarg "You are now an IRC operator"]
  | Rehashing of string                                               [@repr "382"] [@optarg "Rehashing"]
  | YoureService of string                                            [@repr "383"] (*FIXME*)

  | Time of string * string                                           [@repr "391"]
  | UsersStart                                                        [@repr "392"] (*FIXME*)
  | Users of string * string * string                                 [@repr "393"] (*FIXME*)
  | EndOfUsers                                                        [@repr "394"] [@optarg "End of users"]
  | NoUsers                                                           [@repr "395"] [@optarg "Nobody logged in"]

  | WhoisChannels                                                     (*FIXME*)
  | NamReply                                                          (*FIXME*)
  | TraceLink                                                         (*FIXME*)
  | TraceServer                                                       (*FIXME*)
  | StatsOline                                                        (*FIXME*)

[@@deriving irc_internal_ppx { handler_prefix = "on_rpl_" } ]
