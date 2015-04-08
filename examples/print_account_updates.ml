open Core.Std
open Async.Std
open Ibx.Std

let () =
  Command.async_or_error ~summary:"Print account updates"
    Command.Spec.(Common.common_args ())
    (fun do_log host port client_id () ->
      Common.with_tws ~do_log ~host ~port ~client_id (fun tws ->
        Tws.account_updates_exn tws
        >>= fun updates ->
        Pipe.iter_without_pushback updates ~f:(fun update ->
          print_endline (Account_update.sexp_of_t update |> Sexp.to_string_hum)
        )
      )
    )
  |> Command.run
