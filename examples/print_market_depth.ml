open Core.Std
open Async.Std
open Ibx.Std

let () =
  Command.async_basic ~summary:"print market depth"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> Common.client_id_arg ()
      +> Common.duration_arg ()
      +> Common.currency_arg ()
      +> anon ("STOCK-SYMBOL" %: string)
    )
    (fun do_log host port client_id duration currency symbol () ->
      Common.with_tws_client ~do_log ~host ~port ~client_id (fun tws ->
        let stock = Contract.stock ~currency (Symbol.of_string symbol) in
        Tws.market_depth_exn tws ~contract:stock
        >>= fun (book_updates, id) ->
        upon (after duration) (fun () ->
          Tws.cancel_market_depth tws id
        );
        Pipe.iter_without_pushback book_updates ~f:(fun book_update ->
          printf "%s\n%!" (Book_update.sexp_of_t book_update |> Sexp.to_string_hum))
      )
    )
  |> Command.run
