open Core.Std
open Async.Std
open Ibx.Std

module Ascii_table = Core_extended.Ascii_table

let symbols = ["AAPL";"AMZN";"CSCO";"FB";"GOOG";"IBM";"MSFT";"ORCL";"SAP";"YHOO"]

let print_quote_table quotes =
  let module Q = Quote_snapshot in
  let get_symbol    quote = sprintf "%s"    (Q.symbol quote |! Symbol.to_string) in
  let get_bid_size  quote = sprintf "%d"    (Q.bid_size  quote) in
  let get_bid_price quote = sprintf "%4.2f" (Q.bid_price quote |! Price.to_float) in
  let get_ask_size  quote = sprintf "%d"    (Q.ask_size  quote) in
  let get_ask_price quote = sprintf "%4.2f" (Q.ask_price quote |! Price.to_float) in
  let create_col ?(align=Ascii_table.Align.right) =
    Ascii_table.Column.create ~align
  in
  Ascii_table.output ~oc:stdout [
    create_col "Symbol"    get_symbol ~align:Ascii_table.Align.left;
    create_col "Bid size"  get_bid_size;
    create_col "Bid price" get_bid_price;
    create_col "Ask size"  get_ask_size;
    create_col "Ask price" get_ask_price;
  ] quotes

let main ~enable_logging ~host ~port =
  Tws.with_client ~enable_logging ~host ~port
    ~on_handler_error:(`Call (fun e ->
      prerr_endline (Error.to_string_hum e);
      shutdown 1))
    (fun tws ->
      Deferred.all (List.map symbols ~f:(fun symbol ->
        let stock = Contract.stock
          ~exchange:`SMART
          ~currency:`USD
          (Symbol.of_string symbol)
        in
        Tws.quote_snapshot_exn tws ~contract:stock))
      >>| fun quotes -> print_quote_table quotes)

let main_cmd =
  Command.async_basic ~summary:" print market data"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
    )
    (fun enable_logging host port () ->
      if enable_logging then Common.init_logger ();
      main ~enable_logging ~host ~port
    )

let () = Command.run main_cmd
