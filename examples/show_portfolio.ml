open Core.Std
open Async.Std
open Ibx.Std

module Ascii_table = Textutils.Ascii_table

let show_portfolio positions =
  let open Price in
  let module P = Portfolio_position in
  Ascii_table.(output ~oc:stdout ~limit_width_to:120 [
    Column.create "Contract" ~align:Align.left
      (fun pos ->
        sprintf "%s" (P.contract pos |> Contract.to_string));
    Column.create "Exchange" ~align:Align.left
      (fun pos ->
        sprintf "%s" (P.contract pos |> Contract.exchange |> Exchange.to_string));
    Column.create "Position" ~align:Align.right
      (fun pos ->
        sprintf "%d" (P.amount pos));
    Column.create "Currency" ~align:Align.center
      (fun pos ->
        sprintf "%s" (P.contract pos |> Contract.currency |> Currency.to_string));
    Column.create "Market Price" ~align:Align.right
      (fun pos ->
        sprintf "%4.2f" (P.market_price pos :> float));
    Column.create "Market Value" ~align:Align.right
      (fun pos ->
        sprintf "%4.2f" (P.market_value pos :> float));
    Column.create "Avg Cost" ~align:Align.right
      (fun pos ->
        sprintf "%4.2f" (P.average_cost pos :> float));
    Column.create "Total P&L" ~align:Align.right
      (fun pos ->
        sprintf "%4.2f" (P.(realized_pnl pos + unrealized_pnl pos) :> float));
  ] positions)
;;

let () =
  Command.async_or_error ~summary:"show portfolio"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> Common.client_id_arg ()
    )
    (fun do_log host port client_id () ->
      Common.with_tws ~do_log ~host ~port ~client_id (fun tws ->
        Tws.portfolio_positions_exn tws
        >>= fun result ->
        Pipe.to_list result
        >>| fun positions ->
        show_portfolio positions
      )
    )
  |> Command.run
