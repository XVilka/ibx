open Core.Std
open Async.Std
open Ibx.Std

let () =
  Command.async_or_error
    ~summary:"Show Light Sweet Crude Oil (CL) futures chain"
    Command.Spec.(Common.common_args ())
    (fun do_logging host port client_id () ->
      Tws.with_client_or_error ~do_logging ~host ~port ~client_id (fun tws ->
        let cl = Symbol.of_string "CL" in
        Tws.futures_chain_exn tws ~currency:`USD ~exchange:`NYMEX cl
        >>= fun chain ->
        Pipe.iter_without_pushback chain ~f:(fun c ->
          print_endline (Contract.to_string c))
      )
    )
  |> Command.run
