open Core
open Async

let command =
  Command.async ~summary:"Run test suite."
    Command.Spec.(
      empty
      +> flag "-d" no_arg ~doc:" log debug messages"
    )
    (fun debug () ->
       let%bind cwd = Unix.getcwd () in
       let logfile = cwd ^/ "ibx.log" in
       Log.Global.set_output [Log.Output.file `Text ~filename:logfile];
       if debug then Log.Global.set_level `Debug;
       Test_lib.run "ibx" [
         Tws_prot_test.suite
       ; Connection_test.Handshake.suite
       ; Connection_test.Request.suite
       ; Connection_test.Streaming_request.suite
       ; Client_test.suite
       ] >>| fun exit_code -> shutdown exit_code
    )

let () = Command.run command
