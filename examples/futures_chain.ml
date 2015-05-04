open Core.Std
open Async.Std
open Ibx.Std

let () =
  Command.async_or_error
    ~summary:"Show Light Sweet Crude Oil (CL) futures chain"
    Command.Spec.(Common.common_args ())
    (fun do_log host port client_id () ->
      Common.with_tws ~do_log ~host ~port ~client_id (fun tws ->
        let cl = Symbol.of_string "CL" in
        Tws.futures_chain_exn tws ~currency:`USD ~exchange:`NYMEX cl
        >>| fun chain ->
        List.iter chain ~f:(fun c -> print_endline (Contract.to_string c))
      )
    )
  |> Command.run
