open Core.Std
open Async.Std
open Ibx.Std

let () =
  Command.async_or_error
    ~summary:"Show Light Sweet Crude Oil (CL) futures chain"
    Command.Spec.(
      Common.common_args ()
    )
    (fun do_log host port client_id () ->
      Common.with_tws ~do_log ~host ~port ~client_id (fun tws ->
        let cl = Symbol.of_string "CL" in
        Tws.futures_chain_exn tws ~currency:`USD ~exchange:`NYMEX cl
        >>= fun reader ->
        Pipe.to_list reader
        >>| fun futures_chain ->
        List.sort futures_chain ~cmp:(fun c1 c2 ->
          Date.compare (Contract.expiry c1) (Contract.expiry c2)
        ) |> List.iter ~f:(fun c -> print_endline (Contract.to_string c))
      )
    )
  |> Command.run
