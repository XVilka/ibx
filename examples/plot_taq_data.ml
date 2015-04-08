open Core.Std
open Async.Std
open Ibx.Std
open Gnuplot

let verbose = ref true

let plot_taq_data ~duration ~currency ~symbol =
  Common.with_tws (fun tws ->
    let stock = Contract.stock ~currency (Symbol.of_string symbol) in
    Tws.taq_data_exn tws ~contract:stock
    >>= fun (taq_data, id) ->
    upon (Clock.after duration) (fun () ->
      Tws.cancel_quotes tws id
    );
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
    Gp.plot_many gp
      [ Series.steps_timey bids ~title:"bid" ~color:`Green
      ; Series.steps_timey asks ~title:"ask" ~color:`Red
      ; Series.points_timey trades ~title:"trades" ~color:`Blue ];
    Gp.close gp
  )

let () =
  Command.async_or_error ~summary:"Plot trade and quote (TAQ) data"
    Command.Spec.(
      Common.common_args ()
      +> flag "-quiet" no_arg ~doc:" quiet mode"
      +> Common.duration_arg ()
      +> Common.currency_arg ()
      +> anon ("STOCK-SYMBOL" %: string)
    )
    (fun do_log host port client_id quiet duration currency symbol () ->
      verbose := not quiet;
      if Time.Span.(duration > minute) then
        return (Or_error.error_string "Maximum duration is 1 minute")
      else
        plot_taq_data ~do_log ~host ~port ~client_id ~duration ~currency ~symbol)
  |> Command.run
