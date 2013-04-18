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
    Stream.iter (Tws.execution_reports tws) ~f:(fun exec_report ->
      printf "%s\n\n%!"
        (Sexp.to_string_hum (Execution_report.sexp_of_t exec_report)));
    Stream.iter (Tws.commission_reports tws) ~f:(fun comm_report ->
      printf "%s\n\n%!"
        (Sexp.to_string_hum (Commission_report.sexp_of_t comm_report)));
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
      end))

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
