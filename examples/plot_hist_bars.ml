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

let plot_hist_bars ~currency ~symbol =
  Common.with_tws_client (fun tws ->
    Tws.historical_data tws
      ~contract:(Contract.stock ~currency (Symbol.of_string symbol))
      ~bar_size:`Five_mins
      ~duration:(`Day 1)
    >>= function
    | Error e ->
      prerr_endline (Error.to_string_hum e);
      return ()
    | Ok (reader, id) ->
      Pipe.iter_without_pushback reader ~f:(function
      | Error tws_error ->
        begin match Tws_error.error_code tws_error with
        | 162 (* query returned no data *)
        | 200 (* No security definition has been found for the request *) ->
          Tws.cancel_historical_data tws id
        | _   -> ()
        end;
        prerr_endline (Tws_error.to_string_hum tws_error)
      | Ok hist_data ->
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
        Gnuplot.close g;
        Tws.cancel_historical_data tws id))
;;

let command =
  Command.async_basic ~summary:"plot historical bars"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> Common.currency_arg ()
      +> anon ("STOCK-SYMBOL" %: string)
    )
    (fun enable_logging host port currency symbol () ->
      plot_hist_bars ~enable_logging ~host ~port ~currency ~symbol
      >>= function
      | Error e -> prerr_endline (Error.to_string_hum e); exit 1
      | Ok () -> return ()
    )

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
