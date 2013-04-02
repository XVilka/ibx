open Core.Std
open Async.Std
open Ibx.Std

let main ~enable_logging ~host ~port ~currency ~symbol =
  Tws.with_client ~enable_logging ~host ~port
    ~on_handler_error:(`Call (fun e ->
      eprintf "[Error] Failed to retrieve last price for %s:\n%!" symbol;
      prerr_endline (Error.to_string_hum e);
      shutdown 1))
    (fun tws ->
      let stock = Contract.stock ~currency (Symbol.of_string symbol) in
      Tws.trade_snapshot_exn tws ~contract:stock
      >>| fun snapshot ->
      let last_price = Trade_snapshot.last_price snapshot in
      printf "[Info] Last price for %s was %4.2f %s\n"
        symbol (Price.to_float last_price) (Currency.to_string currency))

let main_cmd =
  Command.async_basic ~summary:"Retrieve last stock price"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> Common.currency_arg ()
      +> anon ("STOCK-SYMBOL" %: string)
    )
    (fun enable_logging host port currency symbol () ->
      main ~enable_logging ~host ~port ~currency ~symbol
    )

let () = Command.run main_cmd
