open Core.Std
open Async.Std
open Ibx.Std

module Ascii_table = Textutils.Ascii_table

let show_portfolio positions =
  let module P = Position in
  Ascii_table.(output ~oc:stdout ~limit_width_to:130 [
    Column.create "Contract" ~align:Align.left
      (fun pos ->
        sprintf "%s" (P.contract pos |> Contract.to_string));
    Column.create "Exchange" ~align:Align.left
      (fun pos ->
        sprintf "%s" (P.contract pos |> Contract.exchange |> Exchange.to_string));
    Column.create "Position" ~align:Align.right
      (fun pos ->
        sprintf "%d" (P.volume pos :> int));
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
        sprintf "%4.2f" (P.total_pnl pos :> float));
    Column.create "Return" ~align:Align.right
      (fun pos ->
        sprintf "%2.2f %%" (P.return pos *. 100.));
  ] positions)
;;

let () =
  Command.async_or_error
    ~summary:"Show all portfolio positions"
    Command.Spec.(Common.common_args ())
    (fun do_log host port client_id () ->
      Common.with_tws ~do_log ~host ~port ~client_id (fun tws ->
        Tws.positions_exn tws
        >>= fun result ->
        Pipe.to_list result
        >>| fun positions ->
        show_portfolio positions
      )
    )
  |> Command.run
