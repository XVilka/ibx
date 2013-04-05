open Core.Std
open Async.Std
open Ibx.Std

let print_option_data ~host ~port ~duration =
  Option_client.with_client ~host ~port ~on_handler_error:`Raise (fun clt ->
    (* For current contracts check: http://finance.yahoo.com/q/op?s=GOOG *)
    let goog_call =
      Contract.option
        ~id:(Contract_id.of_string "116034093")
        ~exchange:`CBOE
        ~currency:`USD
        ~option_right:`Call
        ~expiry:(Date.create_exn ~y:2013 ~m:Month.Jun ~d:21)
        ~strike:(Price.of_float 850.)
        (Symbol.of_string "GOOG")
    in
    Option_client.option_data_exn clt ~option:goog_call
    >>= fun (option_ticks, id) ->
    upon (after duration) (fun () -> Option_client.cancel_option_data clt id);
    Pipe.iter_without_pushback option_ticks ~f:(fun option_tick ->
      print_endline (Sexp.to_string_hum (Tick_option.sexp_of_t option_tick)))
  )

let print_option_data_cmd =
  Command.async_basic ~summary:"print option data"
    Command.Spec.(
      empty
      +> flag "-host" (optional_with_default "127.0.0.1" string)
        ~doc:" hostname of TWS or Gateway (default localhost)"
      +> flag "-port" (optional_with_default 4001 int)
        ~doc:" TWS port 7496 or Gateway port 4001 (default 4001)"
      +> flag "-duration" (optional_with_default (sec 30.) time_span)
        ~doc:" duration of the option data stream (default 30s)"
    )
    (fun host port duration () ->
      Monitor.try_with (fun () ->
        print_option_data ~host ~port ~duration
      ) >>= function
      | Error exn ->
        let err = Error.of_exn (Monitor.extract_exn exn) in
        prerr_endline (Error.to_string_hum err);
        exit 1
      | Ok () -> return ())

let () = Command.run print_option_data_cmd
