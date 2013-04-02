open Core.Std
open Async.Std
open Ibx.Std

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

let sma xs ~period = Array.map xs ~f:(unstage (Filter.sma ~period))
let sma10 xs = sma ~period:10 xs
let sma20 xs = sma ~period:20 xs
let sma50 xs = sma ~period:50 xs

let plot_hist_bars ~host ~port ~currency ~symbol =
  Tws.with_client ~host ~port
    ~on_handler_error:(`Call (fun e ->
      prerr_endline (Error.to_string_hum e);
      shutdown 1))
    (fun tws ->
      Tws.historical_data_exn tws
        ~contract:(Contract.stock ~currency (Symbol.of_string symbol))
        ~bar_size:`Five_mins
        ~duration:(`D 1)
      >>| fun hist_data ->
      let module Columns = Historical_data.Columns in
      let hist_data = Historical_data.to_columns hist_data in
      let num_bars  = Array.length (Columns.timestamps hist_data) in
      let indices = Array.init num_bars ~f:Int.to_float in
      let prices = Columns.close_prices hist_data in
      let g = Gnuplot.create () in
      Gnuplot.plot g [
        [ indices;
          Columns.open_prices  hist_data;
          Columns.high_prices  hist_data;
          Columns.low_prices   hist_data;
          Columns.close_prices hist_data; ],
        sprintf "with candlesticks title \"%s\"" symbol;
        [ indices; sma10 prices; ], "w l lc rgb   \"blue\" title \"sma10\"";
        [ indices; sma20 prices; ], "w l lc rgb  \"green\" title \"sma20\"";
        [ indices; sma50 prices; ], "w l lc rgb \"yellow\" title \"sma50\"";
      ];
      Gnuplot.close g)

let plot_hist_bars_cmd =
  Command.async_basic ~summary:"plot historical bars"
    Command.Spec.(
      empty
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> Common.currency_arg ()
      +> anon ("STOCK-SYMBOL" %: string)
    )
    (fun host port currency symbol () ->
      plot_hist_bars ~host ~port ~currency ~symbol
    )

let () = Command.run plot_hist_bars_cmd
