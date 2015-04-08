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

let () =
  Command.async_or_error
    ~summary:"Show detailed contract specifications"
    Command.Spec.(Common.common_args ())
    (fun do_log host port client_id () ->
      Common.with_tws ~do_log ~host ~port ~client_id (fun tws ->
        Deferred.List.iter contracts ~f:(fun contract ->
          let symbol = (Contract.symbol contract :> string) in
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
    )
  |> Command.run
