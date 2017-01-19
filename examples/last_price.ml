open Core.Std
open Async.Std
open Ibx.Std

let () =
  Command.async
    ~summary:"Retrieve the last stock price"
    Command.Spec.(
      Common.common_args ()
      +> Common.currency_arg ()
      +> anon ("STOCK-SYMBOL" %: Arg_type.create Symbol.of_string)
    )
    (fun do_logging host port client_id currency symbol () ->
       Tws.with_client ~do_logging ~host ~port ~client_id
         ~on_handler_error:(`Call (fun e ->
           eprintf "[Error] Failed to retrieve last price for %s: %s\n%!"
             (Symbol.to_string symbol) (Error.to_string_hum e);
         ))
         (fun tws ->
            Tws.latest_trade_exn tws ~contract:(Contract.stock ~currency symbol)
            >>= fun trade ->
            printf "[Info] Last price for %s was %4.2f %s\n"
              (Symbol.to_string symbol) (Trade.price trade |> Price.to_float)
              (Currency.to_string currency);
            return ()
         )
    )
  |> Command.run
