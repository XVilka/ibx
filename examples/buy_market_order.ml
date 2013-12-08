open Core.Std
open Async.Std
open Ibx.Std

let run () =
  let aapl = Contract.stock
    ~exchange:`BATS
    ~currency:`USD
    (Symbol.of_string "AAPL")
  in
  Common.with_tws_client (fun tws ->
    Stream.iter (Tws.executions tws) ~f:(fun execution ->
      printf "%s\n\n%!"
        (Sexp.to_string_hum (Execution.sexp_of_t execution)));
    Stream.iter (Tws.commissions tws) ~f:(fun commission ->
      printf "%s\n\n%!"
        (Sexp.to_string_hum (Commission.sexp_of_t commission)));
    let buy_market = Order.buy_market ~quantity:100 in
    Tws.submit_order_exn tws
      ~contract:aapl
      ~order:buy_market
    >>= fun (order_status, oid) ->
    Pipe.iter_without_pushback order_status ~f:(fun status ->
      printf "%s\n\n%!"
        (Sexp.to_string_hum (Order_status.sexp_of_t status));
      begin
        match Order_status.state status with
        | `Filled ->
          after (sec 0.5) >>> (fun () -> Tws.cancel_order_status tws oid)
        | _ -> ()
      end)
    >>= fun () ->
    Tws.account_and_portfolio_updates_exn tws
    >>= fun updates ->
    Pipe.iter_without_pushback updates ~f:(function
    | `Portfolio_update x ->
      print_endline (Sexp.to_string_hum (Portfolio_update.sexp_of_t x));
    | _ -> ())
  )

let command =
  Command.async_basic ~summary:"submit market buy order for AAPL"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
    )
    (fun enable_logging host port () ->
      run ~enable_logging ~host ~port ()
      >>= function
      | Error e -> prerr_endline (Error.to_string_hum e); exit 1
      | Ok () -> return ()
    )

let () = Command.run command
