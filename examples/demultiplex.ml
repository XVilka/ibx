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
  Common.with_tws (fun tws ->
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

let () =
  Command.async_or_error
    ~summary:"Print market data for three symbols"
    Command.Spec.(Common.common_args () +> Common.duration_arg ())
    (fun do_log host port client_id duration () ->
      print_market_data ~do_log ~host ~port ~client_id ~duration)
  |> Command.run
