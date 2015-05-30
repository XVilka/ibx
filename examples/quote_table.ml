open Core.Std
open Async.Std
open Ibx.Std

module Ascii_table = Textutils.Ascii_table

let print_quote_table quotes =
  let get_symbol    (s, _) = s in
  let get_bid_size  (_, q) = sprintf "%d"    Quote.(bid_size  q :> int) in
  let get_bid_price (_, q) = sprintf "%4.2f" Quote.(bid_price q :> float) in
  let get_ask_price (_, q) = sprintf "%4.2f" Quote.(ask_price q :> float) in
  let get_ask_size  (_, q) = sprintf "%d"    Quote.(ask_size  q :> int) in
  Ascii_table.(output ~oc:stdout [
    Column.create "Symbol"    get_symbol ~align:Align.left;
    Column.create "Bid size"  get_bid_size;
    Column.create "Bid price" get_bid_price;
    Column.create "Ask price" get_ask_price;
    Column.create "Ask size"  get_ask_size;
  ] quotes)

let symbols = ["AAPL";"AMZN";"CSCO";"FB";"GOOG";"IBM";"MSFT";"ORCL";"SAP";"YHOO"]

let () =
  Command.async_or_error
    Command.Spec.(Common.common_args ())
    ~summary:"Tabularize the current quotes for a list of symbols"
    (fun do_logging host port client_id () ->
      Tws.with_client_or_error ~do_logging ~host ~port ~client_id (fun tws ->
        Deferred.List.map symbols ~how:`Parallel ~f:(fun symbol ->
          Tws.contract_details_exn tws
            ~currency:`USD ~sec_type:`Stock (Symbol.of_string symbol)
          >>= fun details ->
          let data = Option.value_exn (Pipe.peek details) in
          (* Extract unambiguous contract description. *)
          let contract = Contract_data.contract data in
          Tws.latest_quote_exn tws ~contract
          >>= fun quote -> return (symbol, quote)
        ) >>| fun quotes -> print_quote_table quotes
      )
    )
  |> Command.run
