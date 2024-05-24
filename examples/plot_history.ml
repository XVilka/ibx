open Core
open Async
open Ibx
open Gnuplot

module Filter = struct
  let sma ~period =
    if period <= 0 then
      invalid_argf "Filter.sma: period must be positive: %d" period ();
    let q = Queue.create () in
    let sum = ref 0.0 in
    stage (fun x_new ->
      sum := !sum +. x_new;
      Queue.enqueue q x_new;
      if Queue.length q > period then begin
        let x_last = Queue.dequeue_exn q in
        sum := !sum -. x_last
      end;
      !sum /. float (Queue.length q))
end

let () =
  Command.async_or_error
    ~summary:"Show a candlestick chart of historical prices"
    Command.Spec.(
      Common.common_args ()
      +> Common.bar_duration_arg ()
      +> Common.sma_period_arg ()
      +> Common.currency_arg ()
      +> Common.timezone_arg ()
      +> anon ("STOCK-SYMBOL" %: Arg_type.create Symbol.of_string)
    )
    (fun do_logging host port client_id duration sma_period currency zone symbol () ->
       Tws.with_client_or_error ~do_logging ~host ~port ~client_id (fun tws ->
         let stock = Contract.stock ~currency symbol in
         Tws.contract_data_exn tws ~contract:stock
         >>= fun data ->
         Tws.history_exn tws ~contract:stock ~duration
         >>| fun history ->
         let zone = Contract_data.time_zone data |> Option.value ~default:zone in
         let start = Time_float_unix.to_date (History.start history) ~zone in
         let stop = Time_float_unix.to_date (History.stop history) ~zone in
         (* Widen the date range to avoid Gnuplot plotting bars on the border. *)
         let start = Date.add_days start (-1) in
         let stop = Date.add_days stop 1 in
         let date_ohlc =
           History.time_ohlc history
           |> List.map ~f:(Tuple2.map_fst ~f:(Time_float_unix.to_date ~zone))
         in
         let gp = Gp.create () in
         Gp.set gp ~title:(Contract_data.long_name data) ~use_grid:true;
         [ (* Create a candlestick chart series. *)
           Series.candles_date_ohlc date_ohlc ~color:`Red |> Option.some
           (* Create a moving average time series of the closing prices. *)
         ; Option.map sma_period ~f:(fun period ->
             let title = sprintf "SMA %d" period in
             let sma = unstage (Filter.sma ~period) in
             List.map date_ohlc ~f:(fun (d, (_,_,_,c)) -> d, sma c)
             |> Series.lines_datey ~title ~color:`Green
           )
         ] |> List.filter_opt |> Gp.plot_many gp ~range:(Range.Date (start, stop));
         Gp.close gp
       )
    )
  |> Command.run
