open Core.Std
open Async.Std
open Ibx.Std

let with_tws ~do_log ~host ~port ~client_id f =
  if do_log then begin
    let basedir = Core.Std.Unix.getcwd () in
    let logfile = basedir ^/ "ibx.log" in
    Log.Global.set_level `Debug;
    Log.Global.set_output [Log.Output.file `Text ~filename:logfile]
  end;
  Monitor.try_with (fun () ->
    Tws.with_client
      ~client_id:(Client_id.of_int_exn client_id)
      ~enable_logging:do_log
      ~host
      ~port
      ~on_handler_error:`Raise
      (fun tws -> f tws)
  )
  >>| fun result ->
  match result with
  | Ok _ as x -> x
  | Error exn -> Or_error.of_exn (Monitor.extract_exn exn)
;;


let common_args () =
  Command.Spec.(
    empty
    +> flag "-enable-logging" no_arg
      ~doc:" enable logging"
    +> flag "-host" (optional_with_default "127.0.0.1" string)
      ~doc:" hostname of TWS or Gateway (default localhost)"
    +> flag "-port" (optional_with_default 4001 int)
      ~doc:" TWS port 7496 or Gateway port 4001 (default 4001)"
    +> flag "-client-id" (optional_with_default 0 int)
      ~doc:" client id of TWS or Gateway (default 0)"
  )

let duration_arg () =
  Command.Spec.(
    flag "-duration"
      (optional_with_default (sec 30.) time_span)
      ~doc:" duration of the data stream (default 30s)"
  )

module Currency = struct
  let of_string x = Currency.t_of_sexp (Sexp.Atom x)
  let arg_type = Command.Spec.Arg_type.create of_string
end

let currency_arg () =
  Command.Spec.(
    flag "-currency" (optional_with_default `USD Currency.arg_type)
      ~doc:" contract's currency"
  )

module Bar_span = struct
  let arg_type = Command.Spec.Arg_type.create Bar_span.of_string
end

let bar_span_arg () =
  Command.Spec.(
    flag "-span" (optional_with_default `Year Bar_span.arg_type)
      ~doc:" the time covered by the historical data request"
  )

module Bar_size = struct
  let arg_type = Command.Spec.Arg_type.create Bar_size.of_string
end

let bar_size_arg () =
  Command.Spec.(
    flag "-size" (optional_with_default `One_day Bar_size.arg_type)
      ~doc:" the size of the bars that will be returned"
  )
