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

let plot_hist_bars ~bar_span ~bar_size ~currency ~symbol =
  Common.with_tws_client (fun tws ->
    Tws.historical_data tws ~bar_span ~bar_size
      ~contract:(Contract.stock ~currency (Symbol.of_string symbol))
    >>= function
    | Error e ->
      prerr_endline (Error.to_string_hum e);
      return ()
    | Ok (reader, id) ->
      Pipe.iter_without_pushback reader ~f:(function
      | Error error ->
        begin match Tws_error.error_code error with
        | 162 (* query returned no data *)
        | 200 (* No security definition has been found for the request *)
        | 321 (* Error validaing request *) ->
          Tws.cancel_historical_data tws id
        | _   -> ()
        end;
        prerr_endline (Tws_error.to_string_hum error)
      | Ok hist_data ->
        let bars = hist_data.bars in
        let sma50 = List.map ~f:(unstage (Filter.sma ~period:50)) in
        let gp = Gp.create () in
        Gp.set gp ~output:(Output.create ~font:"arial" `Wxt);
        Gp.plot_many gp [
          Series.candlesticks ~title:symbol
            (List.map bars ~f:(fun bar ->
              Historical_bar.(stamp bar, (open_ bar, high bar, low bar, close bar)))
             :> (Time.t * (float * float * float * float)) list);
          Series.lines_timey ~color:`Green ~title:"SMA 50"
            (List.zip_exn (List.map bars ~f:Historical_bar.stamp)
               (List.map bars ~f:(fun bar ->
                 bar |> Historical_bar.close |> Price.to_float) |> sma50))
        ];
        Gp.close gp;
        Tws.cancel_historical_data tws id))
;;

let command =
  Command.async_basic ~summary:"plot historical bars"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> Common.client_id_arg ()
      +> Common.currency_arg ()
      +> Common.bar_span_arg ()
      +> Common.bar_size_arg ()
      +> anon ("STOCK-SYMBOL" %: string)
    )
    (fun enable_logging host port client_id currency bar_span bar_size symbol () ->
      plot_hist_bars ~enable_logging ~host ~port ~client_id
        ~currency ~bar_span ~bar_size ~symbol
      >>= function
      | Error e -> prerr_endline (Error.to_string_hum e); exit 1
      | Ok () -> return ()
    )

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
