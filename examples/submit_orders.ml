open Core
open Async
open Ibx

(* NOTE: Before running this program multiple times under the same
   TWS session, please deactivate the checkbox 'Download open orders on
   connection'. In the IB Gateway this checkbox can be found under
   Configure -> Settings -> API Settings.
   Currently IBX cannot handle these open orders messages on connection.
   However, this will be solved in later versions of the library. *)

let submit_and_wait_for_fill tws ~timeout ~contract ~order =
  let order_type = Order_type.to_string (Order.order_type order) in
  printf "Submit %s buy order for %d shares of %s\n%!"
    order_type
    (Order.quantity order :> int)
    (Contract.symbol contract :> string);
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

let () =
  Command.async_or_error ~summary:"Submit a market buy order"
    Command.Spec.(Common.common_args ())
    (fun do_logging host port client_id () ->
       Tws.with_client_or_error ~do_logging ~host ~port ~client_id (fun tws ->
         let symbol = Symbol.of_string "IBM" in
         let ibm = Contract.stock symbol ~exchange:`BATS ~currency:`USD in
         Tws.latest_quote_exn tws ~contract:ibm
         >>= fun quote ->
         let ask_price = Quote.ask_price quote in
         printf "Last ask price %4.2f\n%!" (Price.to_float ask_price);
         let num_shares = Volume.of_int_exn 100 in
         Deferred.List.iter ~how:`Parallel [
           Order.buy_market ~quantity:num_shares;
           Order.buy_limit  ~quantity:num_shares ask_price;
         ] ~f:(fun order ->
           submit_and_wait_for_fill tws ~timeout:(sec 5.) ~contract:ibm ~order
         )
         >>= fun () ->
         Tws.filter_executions_exn tws ~contract:ibm ~action:`Buy
         >>= fun executions ->
         Pipe.iter_without_pushback executions ~f:(fun execution ->
           Format.printf "@[%a@]@\n%!" Execution.pp execution)
       )
    )
  |> Command.run
