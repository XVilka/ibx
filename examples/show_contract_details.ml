open Core.Std
open Async.Std
open Ibx.Std

let contracts : Security_type.t Contract.t list = [
  Contract.stock ~currency:`USD (Symbol.of_string "AAPL");
  Contract.stock ~currency:`EUR (Symbol.of_string "BMW");

  (* For current contracts check:
     http://www.cmegroup.com/trading/equity-index/us-index/
     e-mini-sandp500_product_calendar_futures.html
  *)
  Contract.futures
    ~exchange:`GLOBEX
    ~currency:`USD
    ~expiry:(Date.create_exn ~y:2014 ~m:Month.Mar ~d:21)
    (Symbol.of_string "ES");

  (* For current contracts check: http://finance.yahoo.com/q/op?s=GOOG *)
  Contract.option
    ~exchange:`CBOE
    ~currency:`USD
    ~option_right:`Call
    ~expiry:(Date.create_exn ~y:2013 ~m:Month.Jun ~d:21)
    ~strike:(Price.of_float 850.)
    (Symbol.of_string "GOOG");

  Contract.forex
    ~exchange:`IDEALPRO
    ~currency:`JPY
    (Symbol.of_string "USD");
]

let run () =
  Common.with_tws_client (fun tws ->
    Deferred.List.iter contracts ~f:(fun contract ->
      let symbol = Symbol.to_string (Contract.symbol contract) in
      let message s =
        printf "===== [ %s ] =====\n" symbol;
        print_endline s;
        print_newline ()
      in
      Tws.contract_details tws ~contract >>| function
      | Error e ->
        message (Error.to_string_hum e)
      | Ok (Error tws_error) ->
        message (Tws_error.to_string_hum tws_error)
      | Ok (Ok details) ->
        message (Contract_details.sexp_of_t details |> Sexp.to_string_hum))
  )

let () =
  Command.async_basic ~summary:"show contract details"
    Command.Spec.(
      empty
      +> Common.logging_flag ()
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> Common.client_id_arg ()
    )
    (fun do_log host port client_id () -> run ~do_log ~host ~port ~client_id ())
  |> Command.run
