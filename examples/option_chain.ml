open Core.Std
open Async.Std
open Ibx.Std

let () =
  Command.async_or_error
    ~summary:"Show call option chain for AAPL sorted by expiry and strike"
    Command.Spec.(Common.common_args ())
    (fun do_log host port client_id () ->
      Common.with_tws ~do_log ~host ~port ~client_id (fun tws ->
        let aapl = Symbol.of_string "AAPL" in
        Tws.option_chain_exn tws ~currency:`USD ~option_right:`Call aapl
        >>| fun chain ->
        List.iter chain ~f:(fun c -> print_endline (Contract.to_string c))
      )
    )
  |> Command.run
