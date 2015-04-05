open Core.Std
open Async.Std
open Ibx.Std

let print_last_price ~currency ~symbol =
  Common.with_tws (fun tws ->
    let stock = Contract.stock ~currency (Symbol.of_string symbol) in
    Tws.trade_snapshot tws ~contract:stock
    >>= function
    | Error e ->
      eprintf "[Error] Failed to retrieve last price for %s:\n%!" symbol;
      prerr_endline (Error.to_string_hum e);
      exit 1
    | Ok snapshot ->
      let last_price = Trade_snapshot.last_price snapshot in
      printf "[Info] Last price for %s was %4.2f %s\n"
        symbol (Price.to_float last_price) (Currency.to_string currency);
      return ()
  )

let () =
  Command.async_basic ~summary:"Retrieve last stock price"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> Common.client_id_arg ()
      +> Common.currency_arg ()
      +> anon ("STOCK-SYMBOL" %: string)
    )
    (fun do_log host port client_id currency symbol () ->
      print_last_price ~do_log ~host ~port ~client_id ~currency ~symbol)
  |> Command.run
