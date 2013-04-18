open Core.Std
open Async.Std
open Ibx.Std

let clear () =
  In_thread.run (fun () ->
    try Ok (Core_extended.Shell.run_full "clear" [])
    with exn -> Error exn)

let display_quotes ~duration ~currency ~symbol =
  Common.with_tws_client (fun tws ->
    let symbol = Symbol.of_string symbol in
    Tws.quotes_exn tws ~contract:(Contract.stock ~currency symbol)
    >>= fun (quotes, id) ->
    upon (Clock.after duration) (fun () -> Tws.cancel_quotes tws id);
    clear () >>= fun clear_string ->
    let clear_string =
      match clear_string with
      | Ok s -> s
      | Error _ -> ""
    in
    Pipe.iter_without_pushback quotes ~f:(fun quote ->
      let bid = Quote.bid_price quote in
      let ask = Quote.ask_price quote in
      printf "%s%4.2f-%4.2f\n%!" clear_string
        (Price.to_float bid) (Price.to_float ask)))

let command =
  Command.async_basic ~summary:"displays quotes interactively"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> Common.duration_arg ()
      +> Common.currency_arg ()
      +> anon ("STOCK-SYMBOL" %: string)
    )
    (fun enable_logging host port duration currency symbol () ->
      display_quotes ~enable_logging ~host ~port ~duration ~currency ~symbol
      >>= function
      | Error e -> prerr_endline (Error.to_string_hum e); exit 1
      | Ok () -> return ()
    )

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
