open Core.Std
open Async.Std
open Ibx.Std

module Console = Textutils.Console

let make_tick_printer ~id ~symbol ~color = stage (fun tick ->
  Format.fprintf
    Format.str_formatter "@[<h 0>\\<%s\\>@ id=%s@ symbol=%s@ %a@]"
    (Time.to_string_trimmed ~zone:Time.Zone.local (Time.now ()))
    (Query_id.to_string id)
    (Symbol.to_string symbol)
    Market_data.pp tick;
  Format.close_box ();
  let unescape = unstage (String.Escaping.unescape ~escape_char:'\\') in
  let output = unescape (Format.flush_str_formatter ()) in
  if Console.is_color_tty ()
  then Console.Ansi.printf [`Bright; color] "%s\n%!" output
  else print_endline output;
  return ())

let print_market_data ~duration =
  Common.with_tws_client (fun tws ->
    let print_ticks symbol color =
      let stock = Contract.stock ~currency:`USD symbol in
      Tws.contract_details_exn tws ~contract:stock
      >>= fun details ->
      Tws.market_data_exn tws ~contract:(Contract_details.contract details)
      >>= fun (ticks, id) ->
      upon (Clock.after duration) (fun () -> Tws.cancel_market_data tws id);
      Pipe.iter ticks ~f:(unstage (make_tick_printer ~id ~symbol ~color))
    in
    let symbols = ["AAPL"; "MSFT"; "GOOG"] in
    let colors  = [`Red; `Green; `Blue] in
    Deferred.all_unit (List.map2_exn symbols colors ~f:(fun symbol color ->
      print_ticks (Symbol.of_string symbol) color))
  )

let command =
  Command.async_basic ~summary:" print market data"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> Common.duration_arg ()
      +> Common.client_id_arg ()
    )
    (fun enable_logging host port duration client_id () ->
      print_market_data ~enable_logging ~host ~port ~client_id ~duration
      >>= function
      | Error e -> prerr_endline (Error.to_string_hum e); exit 1
      | Ok () -> return ()
    )

let () = Command.run command
