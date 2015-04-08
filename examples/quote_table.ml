open Core.Std
open Async.Std
open Ibx.Std

module Ascii_table = Textutils.Ascii_table

let print_quote_table quotes =
  let module Q = Quote_snapshot in
  let get_symbol    quote = sprintf "%s"    (Q.symbol quote |> Symbol.to_string) in
  let get_bid_size  quote = sprintf "%d"    (Q.bid_size  quote) in
  let get_bid_price quote = sprintf "%4.2f" (Q.bid_price quote |> Price.to_float) in
  let get_ask_size  quote = sprintf "%d"    (Q.ask_size  quote) in
  let get_ask_price quote = sprintf "%4.2f" (Q.ask_price quote |> Price.to_float) in
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

let symbols = ["AAPL";"AMZN";"CSCO";"FB";"GOOG";"IBM";"MSFT";"ORCL";"SAP";"YHOO"]

let () =
  Command.async_or_error
    Command.Spec.(Common.common_args ())
    ~summary:"Tabularize the current quotes for a list of symbols"
    (fun do_log host port client_id () ->
      Common.with_tws ~do_log ~host ~port ~client_id (fun tws ->
        Deferred.List.map symbols ~how:`Parallel ~f:(fun symbol ->
          let stock = Contract.stock
            ~exchange:`SMART
            ~currency:`USD
            (Symbol.of_string symbol)
          in
          Tws.contract_details_exn tws ~contract:stock
          >>= fun details ->
          (* Extract unambiguous contract description. *)
          let contract = Contract_details.contract details in
          Tws.quote_snapshot_exn tws ~contract)
        >>| fun quotes -> print_quote_table quotes))
  |> Command.run
