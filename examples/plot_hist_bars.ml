open Core.Std
open Async.Std
open Ibx.Std
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
      +> Common.currency_arg ()
      +> Common.bar_span_arg ()
      +> Common.bar_size_arg ()
      +> anon ("STOCK-SYMBOL" %: string)
    )
    (fun do_log host port client_id currency bar_span bar_size symbol () ->
      Common.with_tws ~do_log ~host ~port ~client_id (fun tws ->
        Tws.historical_data_exn tws ~bar_span ~bar_size
          ~contract:(Contract.stock ~currency (Symbol.of_string symbol))
        >>| fun hist_data ->
        let sma50 = unstage (Filter.sma ~period:50) in
        let gp = Gp.create () in
        Gp.plot_many gp ~title:symbol [
          (* Create a candlestick chart series. *)
          Series.candlesticks ~title:"Price"
            (List.map hist_data.bars ~f:(fun bar ->
              Historical_bar.(stamp bar, (op bar, hi bar, lo bar, cl bar)))
             :> (Time.t * (float * float * float * float)) list);
          (* Create a moving average time series of the closing prices. *)
          Series.lines_timey ~color:`Green ~title:"SMA 50"
            (List.map hist_data.bars ~f:(fun bar ->
              Historical_bar.(stamp bar, sma50 (cl bar :> float)))) ];
        Gp.close gp
      )
    )
  |> Command.run
