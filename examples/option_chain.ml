open Core.Std
open Async.Std
open Ibx.Std

let () =
  Command.async_or_error
    ~summary:"Show call option chain for AAPL sorted by expiry and strike"
    Command.Spec.(
      Common.common_args ()
    )
    (fun do_log host port client_id () ->
      Common.with_tws ~do_log ~host ~port ~client_id (fun tws ->
        let sort_by_expiry cs =
          List.sort cs ~cmp:(fun c1 c2 ->
            Date.compare (Contract.expiry c1) (Contract.expiry c2))
        in
        let group_by_expiry cs =
          List.group cs ~break:(fun c1 c2 ->
            Date.((Contract.expiry c1) <> (Contract.expiry c2)))
        in
        let sort_by_strike cs =
          List.sort cs ~cmp:(fun c1 c2 ->
            Price.compare (Contract.strike c1) (Contract.strike c2))
        in
        let flat_map l ~f = List.bind l f in
        let aapl = Symbol.of_string "AAPL" in
        Tws.option_chain_exn tws ~currency:`USD ~option_right:`Call aapl
        >>= fun reader ->
        Pipe.to_list reader
        >>| fun option_chain ->
        sort_by_expiry option_chain
        |> group_by_expiry
        |> flat_map ~f:sort_by_strike
        |> List.iter ~f:(fun c -> print_endline (Contract.to_string c))
      )
    )
  |> Command.run
