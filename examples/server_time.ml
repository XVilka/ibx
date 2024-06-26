open Core
open Async
open Ibx

let () =
  Command.async_or_error ~summary:"Print the current server time"
    Command.Spec.(Common.common_args ())
    (fun do_logging host port client_id () ->
       Tws.with_client_or_error ~do_logging ~host ~port ~client_id (fun tws ->
         Tws.server_time_exn tws >>| fun time ->
         print_endline (Time_float_unix.to_sec_string time ~zone:(Lazy.force Time_float_unix.Zone.local))
       )
    )
  |> Command.run
