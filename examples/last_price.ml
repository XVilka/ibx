open Core.Std
open Async.Std
open Ibx.Std

let () =
  Command.async_or_error
    ~summary:"Retrieve the last stock price"
    Command.Spec.(
      Common.common_args ()
      +> Common.currency_arg ()
      +> anon ("STOCK-SYMBOL" %: string)
    )
    (fun do_log host port client_id currency symbol () ->
      Common.with_tws ~do_log ~host ~port ~client_id (fun tws ->
        let stock = Contract.stock ~currency (Symbol.of_string symbol) in
        Tws.trade_snapshot tws ~contract:stock
        >>= function
        | Error e ->
          eprintf "[Error] Failed to retrieve last price for %s:\n%!" symbol;
          prerr_endline (Error.to_string_hum e);
          exit 1
        | Ok snapshot ->
          let last_price = Trade_snapshot.price snapshot in
          printf "[Info] Last price for %s was %4.2f %s\n"
            symbol (last_price :> float) (Currency.to_string currency);
          return ()
      )
    )
  |> Command.run
