open Core.Std
open Async.Std
open Ibx.Std

let () =
  Command.async_or_error ~summary:"Print real-time bars"
    Command.Spec.(
      Common.common_args ()
      +> Common.duration_arg ()
      +> Common.currency_arg ()
      +> anon ("STOCK-SYMBOL" %: Arg_type.create Symbol.of_string)
    )
    (fun do_logging host port client_id duration currency symbol () ->
      Tws.with_client_or_error ~do_logging ~host ~port ~client_id (fun tws ->
        Tws.realtime_bars_exn tws ~contract:(Contract.stock ~currency symbol )
        >>= fun (bars, id) ->
        upon (after duration) (fun () ->
          Tws.cancel_market_depth tws id
        );
        Pipe.iter_without_pushback bars ~f:(fun bar ->
          printf "%s\n%!" (Bar.sexp_of_t bar |> Sexp.to_string_hum))
      )
    )
  |> Command.run
