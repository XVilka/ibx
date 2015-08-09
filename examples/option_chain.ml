open Core.Std
open Async.Std
open Ibx.Std

let () =
  Command.async_or_error
    ~summary:"Show call option chain for AAPL sorted by expiry and strike"
    Command.Spec.(Common.common_args ())
    (fun do_logging host port client_id () ->
      Tws.with_client_or_error ~do_logging ~host ~port ~client_id (fun tws ->
        let aapl = Symbol.of_string "AAPL" in
        Tws.option_chain_exn tws ~currency:`USD ~right:`Call aapl
        >>| fun chain ->
        List.iter chain ~f:(fun c -> print_endline (Contract.to_string c))
      )
    )
  |> Command.run
