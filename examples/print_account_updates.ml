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

let command =
  Command.async_basic ~summary:"print account updates"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
    )
    (fun enable_logging host port () ->
      print_account_updates ~enable_logging ~host ~port ()
      >>= function
      | Error e -> prerr_endline (Error.to_string_hum e); exit 1
      | Ok () -> return ()
    )

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
