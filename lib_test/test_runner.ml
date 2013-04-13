(* File: test_runner.ml

   IBX - OCaml implementation of the Interactive Brokers TWS API

   Copyright (C) 2013-  Oliver Gu
   email: gu.oliver@yahoo.com

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Core.Std
open Async.Std

let run_cmd = Command.async_basic
  ~summary:"Run test suite."
  Command.Spec.(
    empty
    +> flag "-d" no_arg ~doc:" log debug messages"
  )
  (fun debug () ->
    let cwd = Core.Std.Unix.getcwd () in
    let logfile = cwd ^/ "ibx.log" in
    Log.Global.set_output [Log.Output.file `Text ~filename:logfile];
    if debug then Log.Global.set_level `Debug;
    Test_lib.run "ibx" [
      Pickle_test.suite;
      Connection_test.Handshake.suite;
      Connection_test.Request.suite;
      Connection_test.Streaming_request.suite;
      Client_test.suite;
    ] >>| fun exit_code -> shutdown exit_code
  )

let () = Command.run run_cmd
