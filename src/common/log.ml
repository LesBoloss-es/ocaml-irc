let spf = Format.sprintf

(* let debug s =
 *   Logs_lwt.debug (fun m -> m "%s" s) *)

let debug s =
  Lwt_io.eprintf "%s\n" s

let debug_async s =
  Lwt.async (fun () -> debug s)
