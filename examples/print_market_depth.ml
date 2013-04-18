open Core.Std
open Async.Std
open Ibx.Std

let print_market_depth ~duration ~currency ~symbol =
  Common.with_tws_client (fun tws ->
    let stock = Contract.stock ~currency (Symbol.of_string symbol) in
    Tws.market_depth_exn tws ~contract:stock
    >>= fun (book_updates, id) ->
    upon (after duration) (fun () -> Tws.cancel_market_depth tws id);
    Pipe.iter_without_pushback book_updates ~f:(fun book_update ->
      printf "%s\n%!" (Book_update.sexp_of_t book_update |! Sexp.to_string_hum))
  )

let command =
  Command.async_basic ~summary:"print market depth"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> Common.duration_arg ()
      +> Common.currency_arg ()
      +> anon ("STOCK-SYMBOL" %: string)
    )
    (fun enable_logging host port duration currency symbol () ->
      print_market_depth ~enable_logging ~host ~port ~duration ~currency ~symbol
      >>= function
      | Error e -> prerr_endline (Error.to_string_hum e); exit 1
      | Ok () -> return ()
    )

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
