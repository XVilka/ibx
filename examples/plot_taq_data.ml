open Core.Std
open Async.Std
open Ibx.Std
open Gnuplot

let verbose = ref true

let plot_taq_data ~client_id ~do_logging ~period ~currency ~zone ~symbol =
  Tws.with_client_or_error ~client_id ~do_logging (fun tws ->
    Tws.contract_data_exn tws ~contract:(Contract.stock ~currency symbol)
    >>= fun data ->
    Tws.taq_data_exn tws ~contract:(Contract_data.contract data)
    >>= fun (taq_data, id) ->
    upon (Clock.after period) (fun () -> Tws.cancel_taq_data tws id);
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
    Gp.plot_many gp ~format:"%H:%M:%S"
      ~title:(Contract_data.long_name data) ~use_grid:true
      [ Series.steps_timey bids ~zone ~title:"Bid Price" ~color:`Green
      ; Series.steps_timey asks ~zone ~title:"Ask Price" ~color:`Red
      ; Series.points_timey trades ~zone ~title:"Trades" ~color:`Blue ];
    Gp.close gp
  )

let () =
  Command.async_or_error ~summary:"Plot trade and quote (TAQ) data"
    Command.Spec.(
      Common.common_args ()
      +> flag "-quiet" no_arg ~doc:" quiet mode"
      +> Common.period_arg ()
      +> Common.currency_arg ()
      +> Common.timezone_arg ()
      +> anon ("STOCK-SYMBOL" %: Arg_type.create Symbol.of_string)
    )
    (fun do_logging host port client_id quiet period currency zone symbol () ->
       verbose := not quiet;
       if Time.Span.(period > minute) then
         return (Or_error.error_string "Maximum period is 1 minute")
       else plot_taq_data
           ~do_logging ~host ~port ~client_id ~period ~currency ~zone ~symbol
    )
  |> Command.run
