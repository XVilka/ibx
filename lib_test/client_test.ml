(* File: client_test.ml

   IBX - OCaml implementation of the Interactive Brokers TWS API

   Copyright (C) 2013-  Oliver Gu
   email: gu.oliver@yahoo.com

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Core.Std
open Async.Std
open Test_lib
open Ibx.Std

let server_port  = 10_000
let server_ready = Simulation_server.start_on_port server_port

let with_tws_client handler =
  server_ready
  >>= fun () ->
  Tws.with_client
    ~enable_logging:true
    ~host:"127.0.0.1"
    ~port:server_port
    ~on_handler_error:`Raise
    handler

let suite = "Client" >::: [

  "server-time" >:: (fun () ->
    with_tws_client (fun tws ->
      let module R = Response.Server_time in
      let gen_server_time = Lazy.force Gen.server_time in
      Tws.server_time_exn tws
      >>| fun time ->
      let server_time = R.create ~time in
      assert_response_equal
        (module R : Response_intf.S with type t = R.t)
        ~expected:gen_server_time
        ~actual:server_time;
      Log.Global.sexp ~level:`Debug server_time R.sexp_of_t
    )
  );

  "market-data" >:: (fun () ->
    with_tws_client (fun tws ->
      let module W = Tws.Market_data in
      let gen_market_data = Lazy.force Gen.market_data in
      Tws.market_data_exn tws ~contract:(Rg.contract_g ())
      >>= fun (reader, id) ->
      Pipe.read_exactly reader ~num_values:(List.length gen_market_data)
      >>| fun read_result ->
      Exn.protectx read_result ~f:(function
      | `Eof
      | `Fewer _ -> assert false
      | `Exactly result ->
        List.iter2_exn gen_market_data (Queue.to_list result)
          ~f:(fun gen_tick tick ->
            assert_wrapper_equal
              (module W : Response_intf.Wrapper.S with type t = W.t)
              ~expected:gen_tick
              ~actual:tick;
          Log.Global.sexp ~level:`Debug tick W.sexp_of_t)
      ) ~finally:(fun _ -> Tws.cancel_market_data tws id)
    )
  );

  "calc-option-price" >:: (fun () ->
    with_tws_client (fun tws ->
      let module R = Response.Tick_option in
      let gen_tick_option = Lazy.force Gen.tick_option in
      Tws.calc_option_price_exn tws
        ~contract:(Rg.option_g ())
        ~volatility:(Rg.pfg ())
        ~underlying_price:(Rg.price_g ())
      >>| fun opt_price ->
      let gen_opt_price = Option.value_exn (R.option_price gen_tick_option) in
      assert (Price.(=.) gen_opt_price opt_price);
      Log.Global.sexp ~level:`Debug opt_price Price.sexp_of_t
    )
  );

  "calc-implied-volatility" >:: (fun () ->
    with_tws_client (fun tws ->
      let module R = Response.Tick_option in
      let gen_tick_option = Lazy.force Gen.tick_option in
      Tws.calc_implied_volatility_exn tws
        ~contract:(Rg.option_g ())
        ~option_price:(Rg.price_g ())
        ~underlying_price:(Rg.price_g ())
      >>| fun implied_vol ->
      let gen_implied_vol = Option.value_exn (R.implied_volatility gen_tick_option) in
      assert (Float.(=.) gen_implied_vol implied_vol);
      Log.Global.sexp ~level:`Debug implied_vol <:sexp_of< float >>
    )
  );

  "submit-orders" >:: (fun () ->
    with_tws_client (fun tws ->
      let module R = Response.Order_status in
      let gen_order_states = Lazy.force Gen.order_states in
      Tws.submit_order_exn tws ~contract:(Rg.contract_g ()) ~order:(Rg.order_g ())
      >>= fun (reader, oid) ->
      Pipe.read_exactly reader ~num_values:(List.length gen_order_states)
      >>| fun read_result ->
      Exn.protectx read_result ~f:(function
      | `Eof
      | `Fewer _ -> assert false
      | `Exactly result ->
        List.iter2_exn gen_order_states (Queue.to_list result)
          ~f:(fun gen_order_state order_state ->
            assert_response_equal
              (module R : Response_intf.S with type t = R.t)
              ~expected:gen_order_state
              ~actual:order_state;
          Log.Global.sexp ~level:`Debug order_state R.sexp_of_t)
      ) ~finally:(fun _ -> Tws.cancel_order_status tws oid)
    )
  );

  "account-updates" >:: (fun () ->
    with_tws_client (fun tws ->
      let module R = Response.Account_update in
      let gen_account_updates = Lazy.force Gen.account_updates in
      Tws.account_updates_exn tws
      >>= fun reader ->
      Pipe.read_all reader
      >>| fun result ->
      List.iter2_exn gen_account_updates (Queue.to_list result)
        ~f:(fun gen_account_update account_update ->
          assert_response_equal
            (module R : Response_intf.S with type t = R.t)
            ~expected:gen_account_update
            ~actual:account_update;
          Log.Global.sexp ~level:`Debug account_update R.sexp_of_t)
    )
  );

  "portfolio-positions" >:: (fun () ->
    with_tws_client (fun tws ->
      let module R = Response.Portfolio_position in
      let gen_portfolio_positions = Lazy.force Gen.portfolio_positions in
      Tws.portfolio_positions_exn tws
      >>= fun reader ->
      Pipe.read_all reader
      >>| fun result ->
      List.iter2_exn gen_portfolio_positions (Queue.to_list result)
        ~f:(fun gen_portfolio_position portfolio_position ->
          assert_response_equal
            (module R : Response_intf.S with type t = R.t)
            ~expected:gen_portfolio_position
            ~actual:portfolio_position;
          Log.Global.sexp ~level:`Debug portfolio_position R.sexp_of_t)
    )
  );

  "filter-executions" >:: (fun () ->
    with_tws_client (fun tws ->
      let module R = Response.Execution in
      let gen_executions = Lazy.force Gen.executions in
      Tws.filter_executions_exn tws ~contract:(Rg.contract_g ())
        ~order_action:(Rg.order_action_g ())
      >>= fun reader ->
      Pipe.read_all reader
      >>| fun result ->
      List.iter2_exn gen_executions (Queue.to_list result)
        ~f:(fun gen_execution execution ->
          assert_response_equal
            (module R : Response_intf.S with type t = R.t)
            ~expected:gen_execution
            ~actual:execution;
        Log.Global.sexp ~level:`Debug execution R.sexp_of_t)
    )
  );

  "contract-details" >:: (fun () ->
    with_tws_client (fun tws ->
      let module R = Response.Contract_details in
      let gen_contract_details = Lazy.force Gen.contract_details in
      Tws.contract_details_exn tws ~contract:(Rg.contract_g ())
      >>| fun contract_details ->
      assert_response_equal
        (module R : Response_intf.S with type t = R.t)
        ~expected:gen_contract_details
        ~actual:contract_details;
      Log.Global.sexp ~level:`Debug contract_details R.sexp_of_t
    )
  );

  "market-depth" >:: (fun () ->
    with_tws_client (fun tws ->
      let module R = Response.Book_update in
      let gen_book_updates = Lazy.force Gen.book_updates in
      Tws.market_depth_exn tws ~contract:(Rg.contract_g ())
      >>= fun (reader, id) ->
      Pipe.read_exactly reader ~num_values:(List.length gen_book_updates)
      >>| fun read_result ->
      Exn.protectx read_result ~f:(function
      | `Eof
      | `Fewer _ -> assert false
      | `Exactly result ->
        List.iter2_exn gen_book_updates (Queue.to_list result)
          ~f:(fun gen_book_update book_update ->
            assert_response_equal
              (module R : Response_intf.S with type t = R.t)
              ~expected:gen_book_update
              ~actual:book_update;
            Log.Global.sexp ~level:`Debug book_update R.sexp_of_t)
      ) ~finally:(fun _ -> Tws.cancel_market_depth tws id)
    )
  );

  "historical-data" >:: (fun () ->
    with_tws_client (fun tws ->
      let module R = Response.Historical_data in
      let gen_historical_data = Lazy.force Gen.historical_data in
      Tws.historical_data_exn tws ~contract:(Rg.contract_g ())
      >>| fun historical_data ->
      assert_response_equal
        (module R : Response_intf.S with type t = R.t)
        ~expected:gen_historical_data
        ~actual:historical_data;
      Log.Global.sexp ~level:`Debug historical_data R.sexp_of_t
    )
  );

  "realtime-bars" >:: (fun () ->
    with_tws_client (fun tws ->
      let module R = Response.Realtime_bar in
      let gen_realtime_bars = Lazy.force Gen.realtime_bars in
      Tws.realtime_bars_exn tws ~contract:(Rg.contract_g ())
      >>= fun (reader, id) ->
      Pipe.read_exactly reader ~num_values:(List.length gen_realtime_bars)
      >>| fun read_result ->
      Exn.protectx read_result ~f:(function
      | `Eof
      | `Fewer _ -> assert false
      | `Exactly result ->
        List.iter2_exn gen_realtime_bars (Queue.to_list result)
          ~f:(fun gen_realtime_bar realtime_bar ->
            assert_response_equal
              (module R : Response_intf.S with type t = R.t)
              ~expected:gen_realtime_bar
              ~actual:realtime_bar;
            Log.Global.sexp ~level:`Debug realtime_bar R.sexp_of_t)
      ) ~finally:(fun _ -> Tws.cancel_realtime_bars tws id)
    )
  );
]
