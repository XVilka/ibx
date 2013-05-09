open Core.Std
open Async.Std
open Ibx.Std

(* NOTE: Before running this program multiple times under the same
   TWS session, please deactivate the checkbox 'Download open orders on
   connection'. In the IB Gateway this checkbox can be found under
   Configure -> Settings -> API Settings.
   Currently IBX cannot handle these open orders messages on connection.
   However, this will be solved in later versions of the library. *)

let submit_and_wait_for_fill tws ~timeout ~contract ~order =
  let order_type = Order.Type.to_string (Order.order_type order) in
  printf "Submit %s buy order for %d shares of %s\n%!"
    order_type
    (Order.quantity order)
    (Contract.symbol contract |! Symbol.to_string);
  Tws.submit_order_exn tws ~contract ~order
  >>= fun (order_status, oid) ->
  let wait_for_fill = Pipe.iter order_status ~f:(fun status ->
    match Order_status.state status with
    | `Filled ->
      printf "%s buy order filled at %4.2f\n%!" order_type
        (Price.to_float (Order_status.last_fill_price status));
      Tws.cancel_order_status tws oid; return ()
    | _ -> return ())
  in
  Clock.with_timeout timeout wait_for_fill >>| function
  | `Timeout ->
    printf "Timed out while waiting for fill of %s buy order\n%!" order_type;
    Tws.cancel_order_status tws oid
  | `Result () -> ()

let run ~timeout =
  let ibm = Contract.stock
    ~exchange:`BATS
    ~currency:`USD
    (Symbol.of_string "IBM")
  in
  let num_shares = 100 in
  Common.with_tws_client (fun tws ->
    Tws.quote_snapshot_exn tws ~contract:ibm
    >>= fun snapshot ->
    let ask_price = Quote_snapshot.ask_price snapshot in
    printf "Last ask price %4.2f\n%!" (Price.to_float ask_price);
    Deferred.List.iter ~how:`Parallel [
      Order.buy_market ~quantity:num_shares;
      Order.buy_limit  ~quantity:num_shares ask_price;
    ] ~f:(fun order -> submit_and_wait_for_fill tws ~timeout ~contract:ibm ~order)
    >>= fun () ->
    Tws.filter_executions_exn tws ~contract:ibm ~order_action:`Buy
    >>= fun exec_reports ->
    Pipe.iter_without_pushback exec_reports ~f:(fun exec_report ->
      printf "Execution: \
          exec_id=%s time=%s exchange=%s side=%s shares=%d price=%4.2f\n%!"
        (Execution_report.exec_id exec_report |! Execution_id.to_string)
        (Execution_report.time exec_report |! Time.to_string_trimmed)
        (Execution_report.exchange exec_report |! Exchange.to_string)
        (Execution_report.side exec_report |! Execution_report.Side.to_string)
        (Execution_report.quantity exec_report)
        (Execution_report.price exec_report |! Price.to_float))
    )

let timeout_arg () =
  Command.Spec.(
    flag "-timeout"
      (optional_with_default (sec 5.) time_span)
      ~doc:" timeout on fill (default 5s)"
  )

let command =
  Command.async_basic ~summary:"submit market buy order"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> timeout_arg ()
    )
    (fun enable_logging host port timeout () ->
      run ~enable_logging ~host ~port ~timeout
      >>= function
      | Error e -> prerr_endline (Error.to_string_hum e); exit 1
      | Ok () -> return ()
    )

let () = Command.run command
