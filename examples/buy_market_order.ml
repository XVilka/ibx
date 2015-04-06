open Core.Std
open Async.Std
open Ibx.Std

let () =
  Command.async_basic ~summary:"submit market buy order for AAPL"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> Common.client_id_arg ()
    )
    (fun do_log host port client_id () ->
      Common.with_tws ~do_log ~host ~port ~client_id (fun tws ->
        don't_wait_for (
          Pipe.iter_without_pushback (Tws.executions tws) ~f:(fun exec ->
            printf "%s\n\n%!" (Execution.sexp_of_t exec |> Sexp.to_string_hum))
        );
        don't_wait_for (
          Pipe.iter_without_pushback (Tws.commissions tws) ~f:(fun comm ->
            printf "%s\n\n%!" (Commission.sexp_of_t comm |> Sexp.to_string_hum))
        );
        Tws.submit_order_exn tws
          ~order:(Order.buy_market ~quantity:100)
          ~contract:(Contract.stock (Symbol.of_string "AAPL") ~currency:`USD)
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
        Tws.portfolio_updates_exn tws
        >>= fun updates ->
        Pipe.iter_without_pushback updates ~f:(fun update ->
          print_endline (Portfolio_update.sexp_of_t update |> Sexp.to_string_hum)
        )
      )
    )
  |> Command.run
