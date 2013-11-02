open Core.Std
open Async.Std
open Ibx.Std

let print_portfolio_updates () =
  Common.with_tws_client (fun tws ->
    Tws.account_and_portfolio_updates_exn tws
    >>= fun updates ->
    Pipe.iter_without_pushback updates ~f:(function
      | `Portfolio_update x ->
        print_endline (Sexp.to_string_hum (Portfolio_update.sexp_of_t x));
      | _ -> ()
    )
  )

let command =
  Command.async_basic ~summary:"print portfolio updates"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
    )
    (fun enable_logging host port () ->
      print_portfolio_updates ~enable_logging ~host ~port ()
      >>= function
      | Error e -> prerr_endline (Error.to_string_hum e); exit 1
      | Ok () -> return ()
    )

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
