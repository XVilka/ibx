open Core.Std
open Async.Std
open Ibx.Std

let print_server_time ~port ~host =
  Tws.with_client ~host ~port
    ~on_handler_error:(`Call (fun e ->
      prerr_endline (Error.to_string_hum e);
      shutdown 1))
    (fun tws ->
      Tws.server_time_exn tws
      >>| fun time ->
      print_endline (Time.to_string_trimmed time))

let command =
  Command.async_basic ~summary:"print server time"
    Command.Spec.(
      empty
      +> Common.host_arg ()
      +> Common.port_arg ()
    )
    (fun host port () -> print_server_time ~host ~port)

let () = Command.run command
