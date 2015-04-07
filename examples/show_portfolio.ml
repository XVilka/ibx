open Core.Std
open Async.Std
open Ibx.Std

module Ascii_table = Textutils.Ascii_table

let show_portfolio updates =
  let open Price in
  let module P = Portfolio_update in
  Ascii_table.(output ~oc:stdout ~limit_width_to:120 [
    Column.create "Contract" ~align:Align.left
      (fun update ->
        sprintf "%s" (P.contract update |> Contract.to_string));
    Column.create "Exchange" ~align:Align.left
      (fun update ->
        sprintf "%s" (P.contract update |> Contract.exchange |> Exchange.to_string));
    Column.create "Position" ~align:Align.right
      (fun update ->
        sprintf "%d" (P.position update));
    Column.create "Currency" ~align:Align.center
      (fun update ->
        sprintf "%s" (P.contract update |> Contract.currency |> Currency.to_string));
    Column.create "Market Price" ~align:Align.right
      (fun update ->
        sprintf "%4.2f" (P.market_price update :> float));
    Column.create "Market Value" ~align:Align.right
      (fun update ->
        sprintf "%4.2f" (P.market_value update :> float));
    Column.create "Avg Cost" ~align:Align.right
      (fun update ->
        sprintf "%4.2f" (P.average_cost update :> float));
    Column.create "Total P&L" ~align:Align.right
      (fun update ->
        sprintf "%4.2f" (P.(realized_pnl update + unrealized_pnl update) :> float));
  ] updates)
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
        Tws.portfolio_updates_exn tws
        >>= fun updates -> Pipe.to_list updates >>| show_portfolio
      )
    )
  |> Command.run
