
type t =
  | NoSuchNick of Nickname.t                     [@repr "401"] [@optarg "No such nick/channel"]
  | NoSuchServer of string                       [@repr "402"] [@optarg "No such server"]
  | NoSuchChannel of string                      [@repr "403"] [@optarg "No such channel"]
  | CannotSendToChan of Channel.t                [@repr "404"] [@optarg "Cannot send to channel"]
  | TooManyChannels of Channel.t                 [@repr "405"] [@optarg "You have joined too many channels"]
  | WasNoSuchNick of Nickname.t                  [@repr "406"] [@optarg "There was no such nickname"]
  | TooManyTargets of string * string * string   [@repr "407"] (*FIXME*)
  | NoSuchService of string                      [@repr "408"] [@optarg "No such service"]
  | NoOrigin                                     [@repr "409"] [@optarg "No origin specified"]

  | NoRecipient                                  [@repr "411"] [@optarg "No recipient given"]
  | NoTextToSend                                 [@repr "412"] [@optarg "No text to send"]
  | NoTopLevel of string                         [@repr "413"] [@optarg "No toplevel domain specified"]
  | WildTopLevel of string                       [@repr "414"] [@optarg "Wildcard in toplevel domain"]
  | BadMask of string                            [@repr "415"] [@optarg "Bad server/host mask"]

  | UnknownCommand of string                     [@repr "421"] [@optarg "Unknown command"]
  | NoMotd                                       [@repr "422"] [@optarg "MOTD File is missing"]
  | NoAdminInfo of string                        [@repr "423"] [@optarg "No administrative info available"]
  | FileError of string * string                 [@repr "424"] (*FIXME*)

  | NoNicknameGiven                              [@repr "431"] [@optarg "No nickname given"]
  | ErroneousNickname of string                  [@repr "432"] [@optarg "Erroneous nickname"]
  | NicknameInUse of Nickname.t                  [@repr "433"] [@optarg "Nickname is already in use"]

  | NickCollision of string * string * string    [@repr "436"] (*FIXME*)
  | UnavailResource of string                    [@repr "437"] [@optarg "Nick/channel is temporarily unavailable"]

  | UserNotInChannel of Nickname.t * Channel.t   [@repr "441"] [@optarg "They aren't on that channel"]
  | NotOnChannel of Channel.t                    [@repr "442"] [@optarg "You're not on that channel"]
  | UserOnChannel of Nickname.t * Channel.t      [@repr "443"] [@optarg "is already on channel"]
  | NoLogin of string                            [@repr "444"] [@optarg "User not logged in"]
  | SummonDisabled                               [@repr "445"] [@optarg "SUMMON has been disabled"]
  | UsersDisabled                                [@repr "446"] [@optarg "USERS has been disabled"]

  | NotRegistered                                [@repr "451"] [@optarg "You have not registered"]

  | NeedMoreParams of string                     [@repr "461"] [@optarg "Not enough parameters"]
  | AlreadyRegistered                            [@repr "462"] [@optarg "Unauthorized command (already registered)"]
  | NoPermFromHost                               [@repr "463"] [@optarg "Your host isn't among the privileged"]
  | PasswdMismatch                               [@repr "464"] [@optarg "Password incorrect"]
  | YoureBannedCreep                             [@repr "465"] [@optarg "You are banned from this server"]
  | YouWillBeBanned                              [@repr "466"] [@optarg "You will be banned from this server"]
  | KeySet of Channel.t                          [@repr "467"] [@optarg "Channel key already set"]

  | ChannelIsFull of Channel.t                   [@repr "471"] [@optarg "Cannot join channel (+l)"]
  | UnknownMode of char * Channel.t              [@repr "472"] (*FIXME*)
  | InviteOnlyChan of Channel.t                  [@repr "473"] [@optarg "Cannot join channel (+i)"]
  | BannedFromChan of Channel.t                  [@repr "474"] [@optarg "Cannot join channel (+b)"]
  | BadChannelKey of Channel.t                   [@repr "475"] [@optarg "Cannot join channel (+k)"]
  | BadChanMask of Channel.t                     [@repr "476"] [@optarg "Bad channel mask"]
  | NoChanModes of Channel.t                     [@repr "477"] [@optarg "Channel doesn't support modes"]
  | BanListFull of Channel.t * char              [@repr "478"] [@optarg "Channel list is full"]

  | NoPrivileges                                 [@repr "481"] [@optarg "Permission denied- you're not an IRC operator"]
  | ChanopPrivNeeded of Channel.t                [@repr "482"] [@optarg "You're not channel operator"]
  | CantKillServer                               [@repr "483"] [@optarg "You can't kill a server!"]
  | Restricted                                   [@repr "484"] [@optarg "Your connection is restricted!"]
  | UniqOpPrivNeeded                             [@repr "485"] [@optarg "You're not the original channel operator"]

  | NoOperHost                                   [@repr "491"] [@optarg "No O-lines for your host"]

  | UModeUnknownFlag                             [@repr "501"] [@optarg "Unknown MODE flag"]
  | UsersDontMatch                               [@repr "502"] [@optarg "Cannot change mode for other users"]

[@@deriving irc_internal_ppx]

exception Exception of t
