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
      +> Common.sma_period_arg ()
      +> anon ("STOCK-SYMBOL" %: string)
    )
    (fun do_log host port client_id currency span size period symbol () ->
      Common.with_tws ~do_log ~host ~port ~client_id (fun tws ->
        let stock = Contract.stock ~currency (Symbol.of_string symbol) in
        Tws.history_exn tws ~bar_span:span ~bar_size:size ~contract:stock
        >>| fun history ->
        let gp = Gp.create () in
        Gp.set gp ~use_grid:true;
        [ (* Create a candlestick chart series. *)
          Series.candlesticks ~title:"Price"
            (List.map (History.bars history) ~f:(fun bar ->
              Historical_bar.(stamp bar, (op bar, hi bar, lo bar, cl bar)))
             :> (Time.t * (float * float * float * float)) list) |> Option.some;
          (* Create a moving average time series of the closing prices. *)
          Option.map period ~f:(fun period ->
            let sma = unstage (Filter.sma ~period) in
            Series.lines_timey ~color:`Green ~title:(sprintf "SMA %d" period)
              (List.map (History.bars history) ~f:(fun bar ->
                Historical_bar.(stamp bar, sma (cl bar :> float)))));
        ] |> List.filter_map ~f:Fn.id |> Gp.plot_many gp ~title:symbol;
        Gp.close gp
      )
    )
  |> Command.run
