open Core
open Async
open Test_lib
open Ibx

let server_port  = 10_000
let server_ready = Simulation_server.start_on_port server_port

let with_tws_client handler =
  server_ready
  >>= fun () ->
  Tws.with_client
    ~do_logging:true
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
      Log.Global.sexp ~level:`Debug (R.sexp_of_t server_time)
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
              Log.Global.sexp ~level:`Debug (W.sexp_of_t tick))
      ) ~finally:(fun _ -> Tws.cancel_market_data tws id)
    )
  );

  "option-price" >:: (fun () ->
    with_tws_client (fun tws ->
      let module R = Response.Tick_option in
      let gen_tick_option = Lazy.force Gen.tick_option in
      Tws.option_price_exn tws
        ~contract:(Rg.option_g ())
        ~volatility:(Rg.pfg ())
        ~underlying_price:(Rg.price_g ())
      >>| fun opt_price ->
      let gen_opt_price = R.option_price gen_tick_option in
      assert (Price.(=.) gen_opt_price opt_price);
      Log.Global.sexp ~level:`Debug (Price.sexp_of_t opt_price)
    )
  );

  "implied-volatility" >:: (fun () ->
    with_tws_client (fun tws ->
      let module R = Response.Tick_option in
      let gen_tick_option = Lazy.force Gen.tick_option in
      Tws.implied_volatility_exn tws
        ~contract:(Rg.option_g ())
        ~option_price:(Rg.price_g ())
        ~underlying_price:(Rg.price_g ())
      >>| fun implied_vol ->
      let gen_implied_vol = R.implied_vol gen_tick_option in
      assert (Float.(=.) gen_implied_vol implied_vol);
      Log.Global.sexp ~level:`Debug (Float.sexp_of_t implied_vol)
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
              Log.Global.sexp ~level:`Debug (R.sexp_of_t order_state))
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
          Log.Global.sexp ~level:`Debug (R.sexp_of_t account_update)
        )
    )
  );

  "portfolio" >:: (fun () ->
    with_tws_client (fun tws ->
      let module R = Response.Position in
      let gen_positions = Lazy.force Gen.positions in
      Tws.portfolio_exn tws
      >>= fun reader ->
      Pipe.read_all reader
      >>| fun result ->
      List.iter2_exn gen_positions (Queue.to_list result)
        ~f:(fun gen_position position ->
          assert_response_equal
            (module R : Response_intf.S with type t = R.t)
            ~expected:gen_position
            ~actual:position;
          Log.Global.sexp ~level:`Debug (R.sexp_of_t position))
    )
  );

  "filter-executions" >:: (fun () ->
    with_tws_client (fun tws ->
      let module R = Response.Execution in
      let gen_executions = Lazy.force Gen.executions in
      Tws.filter_executions_exn tws ~contract:(Rg.contract_g ())
        ~action:(Rg.order_action_g ())
      >>= fun reader ->
      Pipe.read_all reader
      >>| fun result ->
      List.iter2_exn gen_executions (Queue.to_list result)
        ~f:(fun gen_execution execution ->
          assert_response_equal
            (module R : Response_intf.S with type t = R.t)
            ~expected:gen_execution
            ~actual:execution;
          Log.Global.sexp ~level:`Debug (R.sexp_of_t execution))
    )
  );

  "contract-details" >:: (fun () ->
    with_tws_client (fun tws ->
      let module R = Response.Contract_data in
      let gen_contract_details = Lazy.force Gen.contract_details in
      Tws.contract_details_exn tws ~sec_type:(Rg.security_type_g ())
        ~currency:(Rg.currency_g ()) (Rg.symbol_g ())
      >>= fun reader ->
      Pipe.read_all reader
      >>| fun result ->
      List.iter2_exn gen_contract_details (Queue.to_list result)
        ~f:(fun gen_contract_data contract_data ->
          assert_response_equal
            (module R : Response_intf.S with type t = R.t)
            ~expected:gen_contract_data
            ~actual:contract_data;
          Log.Global.sexp ~level:`Debug (R.sexp_of_t contract_data))
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
              Log.Global.sexp ~level:`Debug (R.sexp_of_t book_update))
      ) ~finally:(fun _ -> Tws.cancel_market_depth tws id)
    )
  );

  "history" >:: (fun () ->
    with_tws_client (fun tws ->
      let module R = Response.History in
      let gen_history = Lazy.force Gen.history in
      Tws.history_exn tws ~contract:(Rg.contract_g ())
      >>| fun history ->
      assert_response_equal
        (module R : Response_intf.S with type t = R.t)
        ~expected:gen_history
        ~actual:history;
      Log.Global.sexp ~level:`Debug (R.sexp_of_t history)
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
              Log.Global.sexp ~level:`Debug (R.sexp_of_t realtime_bar))
      ) ~finally:(fun _ -> Tws.cancel_realtime_bars tws id)
    )
  );
]
