open Core.Std
open Async.Std
open Ibx.Std

let print_account_updates () =
  Common.with_tws_client (fun tws ->
    Tws.account_updates_exn tws
    >>= fun updates ->
    Pipe.iter_without_pushback updates ~f:(fun update ->
      print_endline (Sexp.to_string_hum (Account_update.sexp_of_t update));
    )
  )

let () =
  Command.async_basic ~summary:"print account updates"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> Common.client_id_arg ()
    )
    (fun do_log host port client_id () ->
      print_account_updates ~do_log ~host ~port ~client_id ())
  |> Command.run
