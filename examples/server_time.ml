open Core.Std
open Async.Std
open Ibx.Std

let print_server_time ~do_log ~port ~host ~client_id =
  Tws.with_client ~enable_logging:do_log ~host ~port
    ~client_id:(Client_id.of_int_exn client_id)
    ~on_handler_error:(`Call (fun e ->
      prerr_endline (Error.to_string_hum e);
      shutdown 1))
    (fun tws ->
      Tws.server_time_exn tws
      >>| fun time ->
      print_endline (Time.to_string_trimmed time ~zone:Time.Zone.local))

let () =
  Command.async
    ~summary:"Print the current server time"
    Command.Spec.(Common.common_args ())
    (fun do_log host port client_id () ->
      print_server_time ~do_log ~host ~port ~client_id)
  |> Command.run
