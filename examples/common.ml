open Core.Std
open Async.Std
open Ibx.Std

let init_logger () =
  let cwd = Core.Std.Unix.getcwd () in
  let logfile = cwd ^/ "ibx.log" in
  Log.Global.set_level `Debug;
  Log.Global.set_output [Log.Output.file `Text ~filename:logfile]

let logging_flag () =
  Command.Spec.(
    flag "-enable-logging" no_arg ~doc:" enable logging"
  )

let host_arg () =
  Command.Spec.(
    flag "-host" (optional_with_default "127.0.0.1" string)
      ~doc:" hostname of TWS or Gateway (default localhost)"
  )

let port_arg () =
  Command.Spec.(
    flag "-port" (optional_with_default 4001 int)
      ~doc:" TWS port 7946 or Gateway port 4001 (default 4001)"
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
