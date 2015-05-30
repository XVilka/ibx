open Core.Std
open Async.Std
open Ibx.Std
open Gnuplot

let verbose = ref true

let plot_taq_data ~client_id ~do_logging ~duration ~currency ~symbol =
  Tws.with_client_or_error ~client_id ~do_logging (fun tws ->
    Tws.taq_data_exn tws ~contract:(Contract.stock ~currency symbol)
    >>= fun (taq_data, id) ->
    upon (Clock.after duration) (fun () ->
      Tws.cancel_taq_data tws id);
    Pipe.fold taq_data ~init:([], [], []) ~f:(fun (bids, asks, trades) taq ->
      if !verbose then Format.printf "@[%a@]@\n%!" TAQ.pp taq;
      return (match taq with
      | TAQ.Trade trade ->
        bids, asks,
        Trade.(stamp trade, price trade) :: trades
      | TAQ.Quote quote ->
        Quote.(stamp quote, bid_price quote) :: bids,
        Quote.(stamp quote, ask_price quote) :: asks,
        trades
      )
    )
    >>| fun (bids, asks, trades) ->
    (* Reverse accumulated quotes and trades and use coercion on price types. *)
    let bids = List.rev (bids : ('t * Price.t) list :> ('t * float) list) in
    let asks = List.rev (asks : ('t * Price.t) list :> ('t * float) list) in
    let trades = List.rev (trades : ('t * Price.t) list :> ('t * float) list) in
    let gp = Gp.create () in
    Gp.plot_many gp ~title:(Symbol.to_string symbol)
      [ Series.steps_timey bids ~title:"Bid Price" ~color:`Green
      ; Series.steps_timey asks ~title:"Ask Price" ~color:`Red
      ; Series.points_timey trades ~title:"Trades" ~color:`Blue ];
    Gp.close gp
  )

let () =
  Command.async_or_error ~summary:"Plot trade and quote (TAQ) data"
    Command.Spec.(
      Common.common_args ()
      +> flag "-quiet" no_arg ~doc:" quiet mode"
      +> Common.duration_arg ()
      +> Common.currency_arg ()
      +> anon ("STOCK-SYMBOL" %: Arg_type.create Symbol.of_string)
    )
    (fun do_logging host port client_id quiet duration currency symbol () ->
      verbose := not quiet;
      if Time.Span.(duration > minute) then
        return (Or_error.error_string "Maximum duration is 1 minute")
      else plot_taq_data ~do_logging ~host ~port ~client_id ~duration
        ~currency ~symbol
    )
  |> Command.run
