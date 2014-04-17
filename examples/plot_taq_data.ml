open Core.Std
open Async.Std
open Ibx.Std

let verbose = ref true

let plot_taq_data ~duration ~currency ~symbol =
  Common.with_tws_client (fun tws ->
    let stock = Contract.stock ~currency (Symbol.of_string symbol) in
    Tws.taq_data_exn tws ~contract:stock
    >>= fun (taq_data, id) ->
    upon (Clock.after duration) (fun () -> Tws.cancel_quotes tws id);
    Pipe.fold taq_data ~init:([], [], [], [], [])
      ~f:(fun (ttms, tpxs, qtms, asks, bids) taq_record ->
        if !verbose then Format.printf "@[%a@]@\n%!" TAQ.pp taq_record;
        return (match taq_record with
        | TAQ.Trade trade ->
          Trade.time  trade :: ttms,
          Trade.price trade :: tpxs,
          qtms, asks, bids
        | TAQ.Quote quote ->
          ttms, tpxs,
          Quote.time      quote :: qtms,
          Quote.ask_price quote :: asks,
          Quote.bid_price quote :: bids))
    >>| fun (ttms, tpxs, qtms, asks, bids) ->
    let ttms = Array.of_list_rev (ttms : Time.t  list :> float list) in
    let tpxs = Array.of_list_rev (tpxs : Price.t list :> float list) in
    let qtms = Array.of_list_rev (qtms : Time.t  list :> float list) in
    let asks = Array.of_list_rev (asks : Price.t list :> float list) in
    let bids = Array.of_list_rev (bids : Price.t list :> float list) in
    let g = Gnuplot.create () in
    Gnuplot.send g (sprintf "set title \"%s\"" symbol);
    Gnuplot.send g "unset xtics";
    Gnuplot.plot g [
      [ttms; tpxs], "with points lc rgb 'blue'  title \"trades\"";
      [qtms; asks], "with steps  lc rgb 'red'   title \"ask\"";
      [qtms; bids], "with steps  lc rgb 'green' title \"bid\"";
    ];
    Gnuplot.close g)

let command =
  Command.async_basic ~summary:"plot TAQ data"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> flag "-quiet" no_arg ~doc:" quiet mode"
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> Common.client_id_arg ()
      +> Common.duration_arg ()
      +> Common.currency_arg ()
      +> anon ("STOCK-SYMBOL" %: string)
    )
    (fun enable_logging quiet host port client_id duration currency symbol () ->
      verbose := not quiet;
      if Time.Span.(duration > minute) then begin
        prerr_endline "Maximum duration is 1 minute";
        exit 1;
      end else
        plot_taq_data ~enable_logging ~host ~port ~client_id
          ~duration ~currency ~symbol
        >>= function
        | Error e -> prerr_endline (Error.to_string_hum e); exit 1
        | Ok () -> return ()
    )

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
