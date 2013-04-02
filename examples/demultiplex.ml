open Core.Std
open Core_extended.Std
open Async.Std
open Ibx.Std

let make_tick_printer id symbol ~color = stage (fun tick ->
  Format.fprintf Format.str_formatter "@[<h 0>\\<%s\\>@ id=%s@ symbol=%s@ %a@]"
    (Time.to_string (Time.now ()))
    (Query_id.to_string id)
    (Symbol.to_string symbol)
    Market_data.pp tick;
  Format.close_box ();
  let unescape = unstage (String.Escaping.unescape ~escape_char:'\\') in
  let output = unescape (Format.flush_str_formatter ()) in
  if Console.is_color_tty () then
    Console.Ansi.string_with_attr [`Bright; color] output
  else output)

let print_market_data ~enable_logging ~host ~port ~duration =
  Tws.with_client ~enable_logging ~host ~port ~on_handler_error:`Raise (fun tws ->
    let process_market_data symbol color =
      Tws.market_data tws ~contract:(Contract.stock ~currency:`USD symbol)
      >>| function
      | Error e ->
        prerr_endline (Error.to_string_hum e);
        let pipe_r, pipe_w = Pipe.create () in
        Pipe.close pipe_w;
        pipe_r
      | Ok (ticks, id) ->
        upon (Clock.after duration) (fun () -> Tws.cancel_market_data tws id);
        Pipe.map ticks ~f:(unstage (make_tick_printer id symbol ~color))
    in
    let symbols = ["AAPL"; "MSFT"; "GOOG"] in
    let colors  = [`Red; `Green; `Blue] in
    Deferred.all (List.map2_exn symbols colors ~f:(fun symbol color ->
      process_market_data (Symbol.of_string symbol) color))
    >>= fun pipes ->
    Pipe.iter_without_pushback (Pipe.interleave pipes) ~f:print_endline)

let print_market_data_cmd  =
  Command.async_basic ~summary:" print market data"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> Common.duration_arg ()
    )
    (fun enable_logging host port duration () ->
      if enable_logging then Common.init_logger ();
      Monitor.try_with (fun () ->
        print_market_data ~enable_logging ~host ~port ~duration
      ) >>| function
      | Error exn ->
        let err = Error.of_exn (Monitor.extract_exn exn) in
        prerr_endline (Error.to_string_hum err);
        shutdown 1
      | Ok () -> ())

let () = Command.run print_market_data_cmd
