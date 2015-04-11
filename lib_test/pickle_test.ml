(* File: pickle_test.ml

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
open Ibx.Std
open Test_lib
open Tws_prot

let to_tws = Pickler.run
let of_tws = Unpickler.run_exn

let input_of_output output =
  let truncate_null s = String.sub s ~pos:0 ~len:(String.length s - 1) in
  Queue.of_list (String.split ~on:'\000' (truncate_null output))

module Query = struct

  let gen_test (type s) query query_g =
    let expected = query_g () in
    let module Q = (val query : Query_intf.S with type t = s) in
    let output = to_tws Q.pickler expected in
    let actual = of_tws (Only_in_test.force Q.unpickler) (input_of_output output) in
    assert_query_equal query ~expected ~actual;
    Deferred.unit

  let tests = [

    (* ====================== Connection and server ======================== *)

    (fun () ->
      let module Q = Query.Server_log_level in
      gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.server_log_level_g);

    (fun () ->
      let module Q = Query.Server_time in
      gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.server_time_g);

    (* =========================== Market data ============================= *)

    (fun () ->
      let module Q = Query.Market_data in
      gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.market_data_g);

    (fun () ->
      let module Q = Query.Calc_option_price in
      gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.calc_option_price_g);

    (fun () ->
      let module Q = Query.Calc_implied_volatility in
      gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.calc_implied_volatility_g);

    (* ============================= Orders ================================ *)

    (fun () ->
      let module Q = Query.Submit_order in
      gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.submit_order_g);

    (* ====================== Account and portfolio ======================== *)

    (fun () ->
      let module Q = Query.Account_updates in
      gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.account_updates);

    (fun () ->
      let module Q = Query.Portfolio_positions in
      gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.portfolio_positions);

    (* ============================ Executions ============================= *)

    (fun () ->
      let module Q = Query.Executions in
      gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.executions_g);

    (* ========================= Contract details ========================== *)

    (fun () ->
      let module Q = Query.Contract_details in
      gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.contract_details_g);

    (* =========================== Market depth ============================ *)

    (fun () ->
      let module Q = Query.Market_depth in
      gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.market_depth_g);

    (* ========================= Historical data =========================== *)

    (fun () ->
      let module Q = Query.Historical_data in
      gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.historical_data_g);

    (* ========================== Realtime bars ============================ *)

    (fun () ->
      let module Q = Query.Realtime_bars in
      gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.realtime_bars_g);
  ]
end

module Response = struct

  let gen_test (type s) response response_g =
    let expected = response_g () in
    let module R = (val response : Response_intf.S with type t = s) in
    let output = to_tws (Only_in_test.force R.pickler) expected in
    let actual = of_tws R.unpickler (input_of_output output) in
    assert_response_equal response ~expected ~actual;
    Deferred.unit

  let tests = [

    (* ====================== Connection and server ======================== *)

    (fun () ->
      let module R = Response.Tws_error in
      gen_test (module R : Response_intf.S with type t = R.t) Rg.R.tws_error_g);

    (fun () ->
      let module R = Response.Server_time in
      gen_test (module R : Response_intf.S with type t = R.t) Rg.R.server_time_g);

    (* =========================== Market data ============================= *)

    (fun () ->
      let module R = Response.Tick_price in
      gen_test (module R : Response_intf.S with type t = R.t) Rg.R.tick_price_g);

    (fun () ->
      let module R = Response.Tick_size in
      gen_test (module R : Response_intf.S with type t = R.t) Rg.R.tick_size_g);

    (fun () ->
      let module R = Response.Tick_option in
      gen_test (module R : Response_intf.S with type t = R.t) Rg.R.tick_option_g);

    (fun () ->
      let module R = Response.Tick_string in
      gen_test (module R : Response_intf.S with type t = R.t) Rg.R.tick_string_g);

    (* ============================= Orders ================================ *)

    (fun () ->
      let module R = Response.Order_status in
      gen_test (module R : Response_intf.S with type t = R.t) Rg.R.order_status_g);

    (* ======================= Account and Portfolio ======================= *)

    (fun () ->
      let module R = Response.Account_update in
      gen_test (module R : Response_intf.S with type t = R.t) Rg.R.account_update_g);

    (fun () ->
      let module R = Response.Portfolio_position in
      gen_test (module R : Response_intf.S with type t = R.t) Rg.R.portfolio_position_g);

    (* ========================= Contract details ========================== *)

    (fun () ->
      let module R = Response.Contract_details in
      gen_test (module R : Response_intf.S with type t = R.t) Rg.R.contract_details_g);

    (* =========================== Executions ============================== *)

    (fun () ->
      let module R = Response.Execution in
      gen_test (module R : Response_intf.S with type t = R.t) Rg.R.execution_g);

    (* =========================== Market depth ============================ *)

    (fun () ->
      let module R = Response.Book_update in
      gen_test (module R : Response_intf.S with type t = R.t) Rg.R.book_update_g);

    (* ========================= Historical data =========================== *)

    (fun () ->
      let module R = Response.Historical_data in
      gen_test (module R : Response_intf.S with type t = R.t) Rg.R.historical_data_g);

    (* ========================== Realtime bars ============================ *)

    (fun () ->
      let module R = Response.Realtime_bar in
      gen_test (module R : Response_intf.S with type t = R.t) Rg.R.realtime_bar_g);
  ]
end

let test_all tests = fun () ->
  Deferred.all_unit (List.map tests ~f:(fun f -> f ()))

let suite = "Pickle" >::: [
  "query-pickle-unpickle" >:: (test_all Query.tests);
  "response-pickle-unpickle" >:: (test_all Response.tests);
]
