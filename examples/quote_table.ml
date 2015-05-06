open Core.Std
open Async.Std
open Ibx.Std

module Ascii_table = Textutils.Ascii_table

let print_quote_table quotes =
  let module Q = Quote_snapshot in
  let get_symbol    (s, _) = sprintf "%s"    s in
  let get_bid_size  (_, q) = sprintf "%d"    Q.(bid_size  q :> int) in
  let get_bid_price (_, q) = sprintf "%4.2f" Q.(bid_price q :> float) in
  let get_ask_size  (_, q) = sprintf "%d"    Q.(ask_size  q :> int) in
  let get_ask_price (_, q) = sprintf "%4.2f" Q.(ask_price q :> float) in
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
          Tws.contract_details_exn tws ~currency:`USD
            ~security_type:`Stock (Symbol.of_string symbol)
          >>= fun details ->
          let data = Option.value_exn (Pipe.peek details) in
          (* Extract unambiguous contract description. *)
          let contract = Contract_data.contract data in
          Tws.quote_snapshot_exn tws ~contract
          >>= fun quote -> return (symbol, quote)
        ) >>| fun quotes -> print_quote_table quotes
      )
    )
  |> Command.run
