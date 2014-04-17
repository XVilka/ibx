open Core.Std
open Async.Std
open Ibx.Std

let with_tws_client ~enable_logging ~host ~port ~client_id f =
  if enable_logging then begin
    let basedir = Core.Std.Unix.getcwd () in
    let logfile = basedir ^/ "ibx.log" in
    Log.Global.set_level `Debug;
    Log.Global.set_output [Log.Output.file `Text ~filename:logfile]
  end;
  Monitor.try_with (fun () ->
    Tws.with_client
      ~client_id:(Client_id.of_int_exn client_id)
      ~enable_logging
      ~host
      ~port
      ~on_handler_error:`Raise
      (fun tws -> f tws)
  )
  >>| fun result ->
  match result with
  | Ok _ as x -> x
  | Error exn -> Error (Error.of_exn (Monitor.extract_exn exn))

let logging_flag () =
  Command.Spec.(
    flag "-enable-logging" no_arg ~doc:" enable logging"
  )

let client_id_arg () =
  Command.Spec.(
    flag "-client-id" (optional_with_default 0 int)
      ~doc:" client id of TWS or Gateway (default 0)"
  )

let host_arg () =
  Command.Spec.(
    flag "-host" (optional_with_default "127.0.0.1" string)
      ~doc:" hostname of TWS or Gateway (default localhost)"
  )

let port_arg () =
  Command.Spec.(
    flag "-port" (optional_with_default 4001 int)
      ~doc:" TWS port 7496 or Gateway port 4001 (default 4001)"
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
