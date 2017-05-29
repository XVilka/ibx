(* File: tws_prot_test.ml

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

open Core
open Async
open Ibx
open Test_lib
open Tws_prot

let to_tws = Encoder.run
let of_tws = Decoder.run_exn

let input_of_output output =
  let truncate_null s = String.sub s ~pos:0 ~len:(String.length s - 1) in
  Queue.of_list (String.split ~on:'\000' (truncate_null output))

module Query = struct

  let gen_test (type s) query query_g =
    let expected = query_g () in
    let module Q = (val query : Query_intf.S with type t = s) in
    let output = to_tws Q.encoder expected in
    let actual = of_tws (Only_in_test.force Q.decoder) (input_of_output output) in
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
       let module Q = Query.Option_price in
       gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.option_price_g);

    (fun () ->
       let module Q = Query.Implied_volatility in
       gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.implied_volatility_g);

    (* ============================= Orders ================================ *)

    (fun () ->
       let module Q = Query.Submit_order in
       gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.submit_order_g);

    (* ====================== Account and portfolio ======================== *)

    (fun () ->
       let module Q = Query.Account_updates in
       gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.account_updates);

    (fun () ->
       let module Q = Query.Positions in
       gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.positions);

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

    (* ============================= History ============================= *)

    (fun () ->
       let module Q = Query.History in
       gen_test (module Q : Query_intf.S with type t = Q.t) Rg.Q.history_g);

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
    let output = to_tws (Only_in_test.force R.encoder) expected in
    let actual = of_tws R.decoder (input_of_output output) in
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
       let module R = Response.Position in
       gen_test (module R : Response_intf.S with type t = R.t) Rg.R.position_g);

    (* =========================== Contract data =========================== *)

    (fun () ->
       let module R = Response.Contract_data in
       gen_test (module R : Response_intf.S with type t = R.t) Rg.R.contract_data_g);

    (* =========================== Executions ============================== *)

    (fun () ->
       let module R = Response.Execution in
       gen_test (module R : Response_intf.S with type t = R.t) Rg.R.execution_g);

    (* =========================== Market depth ============================ *)

    (fun () ->
       let module R = Response.Book_update in
       gen_test (module R : Response_intf.S with type t = R.t) Rg.R.book_update_g);

    (* ============================= History =============================== *)

    (fun () ->
       let module R = Response.History in
       gen_test (module R : Response_intf.S with type t = R.t) Rg.R.history_g);
  ]
end

let test_all tests = fun () -> Deferred.all_unit (List.map tests ~f:(fun f -> f ()))

let suite = "Tws_prot" >::: [
  "query-encode-decode" >:: (test_all Query.tests);
  "response-encode-decode" >:: (test_all Response.tests);
]
