open Core.Std
open Async.Std
open Ibx.Std

let print_market_depth ~host ~port ~duration ~currency ~symbol =
  Tws.with_client ~host ~port ~on_handler_error:`Raise (fun tws ->
    let stock = Contract.stock ~currency (Symbol.of_string symbol) in
    Tws.market_depth_exn tws ~contract:stock
    >>= fun (book_updates, id) ->
    upon (after duration) (fun () -> Tws.cancel_market_depth tws id);
    Pipe.iter_without_pushback book_updates ~f:(fun book_update ->
      printf "%s\n%!" (Book_update.sexp_of_t book_update |! Sexp.to_string_hum))
  )

let print_market_depth_cmd =
  Command.async_basic ~summary:"print market depth"
    Command.Spec.(
      empty
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> Common.duration_arg ()
      +> Common.currency_arg ()
      +> anon ("STOCK-SYMBOL" %: string)
    )
    (fun host port duration currency symbol () ->
      Monitor.try_with (fun () ->
        print_market_depth ~host ~port ~duration ~currency ~symbol
      ) >>= function
      | Error exn ->
        let err = Error.of_exn (Monitor.extract_exn exn) in
        prerr_endline (Error.to_string_hum err);
        exit 1
      | Ok () -> return ())

let () = Command.run print_market_depth_cmd
