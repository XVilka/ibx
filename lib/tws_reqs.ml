(* File: tws_reqs.ml

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

open Tws_prot

module U = Unpickler
module S = Send_tag
module R = Recv_tag

(* ==================== Connection and server ===================== *)

let req_server_time = Ib.Request.create
  ~send_header:(Ib.Header.create ~tag:S.Server_time ~version:1)
  ~recv_header:(Ib.Header.create ~tag:R.Server_time ~version:1)
  ~tws_query:Query.Server_time.pickler
  ~tws_response:Response.Server_time.unpickler

(* ========================= Market data ========================== *)

let req_market_data = Ib.Streaming_request.create
  ~send_header:(Ib.Header.create ~tag:S.Market_data ~version:9)
  ~canc_header:(Ib.Header.create ~tag:S.Cancel_market_data ~version:1)
  ~recv_header:[
    Ib.Header.create ~tag:R.Tick_price ~version:6;
    Ib.Header.create ~tag:R.Tick_size ~version:6;
    Ib.Header.create ~tag:R.Tick_option ~version:6;
    Ib.Header.create ~tag:R.Tick_string ~version:6;
  ]
  ~skip_header:[
    Ib.Header.create ~tag:R.Tick_generic ~version:6;
    Ib.Header.create ~tag:R.Snapshot_end ~version:1
  ]
  ~tws_query:Query.Market_data.pickler
  ~tws_response:[
    U.map Response.Tick_price.unpickler  ~f:(fun x -> `Tick_price  x);
    U.map Response.Tick_size.unpickler   ~f:(fun x -> `Tick_size   x);
    U.map Response.Tick_option.unpickler ~f:(fun x -> `Tick_option x);
    U.map Response.Tick_string.unpickler ~f:(fun x -> `Tick_string x);
  ] ()

let req_calc_option_price = Ib.Streaming_request.create
  ~send_header:(Ib.Header.create ~tag:S.Calc_option_price ~version:1)
  ~canc_header:(Ib.Header.create ~tag:S.Cancel_calc_option_price ~version:1)
  ~recv_header:[Ib.Header.create ~tag:R.Tick_option ~version:6]
  ~tws_query:Query.Calc_option_price.pickler
  ~tws_response:[
    U.map Response.Tick_option.unpickler ~f:Response.Tick_option.option_price;
  ] ()

let req_calc_implied_volatility = Ib.Streaming_request.create
  ~send_header:(Ib.Header.create ~tag:S.Calc_implied_volatility ~version:1)
  ~canc_header:(Ib.Header.create ~tag:S.Cancel_calc_implied_volatility ~version:1)
  ~recv_header:[Ib.Header.create ~tag:R.Tick_option ~version:6]
  ~tws_query:Query.Calc_implied_volatility.pickler
  ~tws_response:[
    U.map Response.Tick_option.unpickler ~f:Response.Tick_option.implied_volatility;
  ] ()

(* ===================== Contract details ========================= *)

let req_contract_details = Ib.Streaming_request.create
  ~send_header:(Ib.Header.create ~tag:S.Contract_data ~version:6)
  ~recv_header:[
    Ib.Header.create ~tag:R.Contract_data ~version:8;
    Ib.Header.create ~tag:R.Contract_data_end ~version:1
  ]
  ~tws_query:Query.Contract_details.pickler
  ~tws_response:[
    U.map Response.Contract_data.unpickler ~f:(fun x -> `Contract_data x);
    U.const `Contract_data_end
  ] ()

(* ========================== Orders ============================== *)

let req_submit_order = Ib.Streaming_request.create
  ~send_header:(Ib.Header.create ~tag:S.Submit_order ~version:39)
  ~canc_header:(Ib.Header.create ~tag:S.Cancel_order ~version:1)
  ~recv_header:[Ib.Header.create ~tag:R.Order_status ~version:6]
  ~skip_header:[Ib.Header.create ~tag:R.Open_order ~version:30]
  ~tws_query:Query.Submit_order.pickler
  ~tws_response:[Response.Order_status.unpickler]
  ()

(* ================== Account and portfolio ======================= *)

let req_account_updates = Ib.Streaming_request_without_id.create
  ~send_header:(Ib.Header.create ~tag:S.Account_data ~version:2)
  ~recv_header:[
    Ib.Header.create ~tag:R.Account_update ~version:2;
    Ib.Header.create ~tag:R.Account_download_end ~version:1;
  ]
  ~skip_header:[
    Ib.Header.create ~tag:R.Account_update_time ~version:1;
    Ib.Header.create ~tag:R.Portfolio_position ~version:7;
  ]
  ~tws_query:Query.Account_updates.pickler
  ~tws_response:[
    U.map Response.Account_update.unpickler ~f:(fun x -> `Update x);
    U.map Account_code.unpickler ~f:(fun x -> `Update_end x);
  ] ()

let req_portfolio_updates = Ib.Streaming_request_without_id.create
  ~send_header:(Ib.Header.create ~tag:S.Account_data ~version:2)
  ~recv_header:[
    Ib.Header.create ~tag:R.Portfolio_position ~version:7;
    Ib.Header.create ~tag:R.Account_download_end ~version:1;
  ]
  ~skip_header:[
    Ib.Header.create ~tag:R.Account_update ~version:2;
    Ib.Header.create ~tag:R.Account_update_time ~version:1;
  ]
  ~tws_query:Query.Portfolio_positions.pickler
  ~tws_response:[
    U.map Response.Portfolio_position.unpickler ~f:(fun x -> `Update x);
    U.map Account_code.unpickler ~f:(fun x -> `Update_end x);
  ] ()

(* ========================= Executions =========================== *)

let req_executions = Ib.Streaming_request.create
  ~send_header:(Ib.Header.create ~tag:Send_tag.Executions ~version:3)
  ~recv_header:[
    Ib.Header.create ~tag:R.Execution ~version:9;
    Ib.Header.create ~tag:R.Executions_end ~version:1;
  ]
  ~tws_query:Query.Executions.pickler
  ~tws_response:[
    U.map Response.Execution.unpickler ~f:(fun x -> `Execution x);
    U.const `Executions_end;
  ] ()

(* ======================== Market depth ========================== *)

let req_market_depth = Ib.Streaming_request.create
  ~send_header:(Ib.Header.create ~tag:S.Market_depth ~version:3)
  ~canc_header:(Ib.Header.create ~tag:S.Cancel_market_depth ~version:1)
  ~recv_header:[Ib.Header.create ~tag:R.Book_update ~version:1]
  ~skip_header:[Ib.Header.create ~tag:R.Book_update_L2 ~version:1]
  ~tws_query:Query.Market_depth.pickler
  ~tws_response:[Response.Book_update.unpickler]
  ()

(* ====================== Historical data ========================= *)

let req_historical_data = Ib.Streaming_request.create
  ~send_header:(Ib.Header.create ~tag:S.Historical_data ~version:4)
  ~canc_header:(Ib.Header.create ~tag:S.Cancel_historical_data ~version:1)
  ~recv_header:[Ib.Header.create ~tag:R.Historical_data ~version:3]
  ~tws_query:Query.Historical_data.pickler
  ~tws_response:[Response.Historical_data.unpickler]
  ()

(* ======================== Realtime bars ========================= *)

let req_realtime_bars = Ib.Streaming_request.create
  ~send_header:(Ib.Header.create ~tag:S.Realtime_bars ~version:1)
  ~canc_header:(Ib.Header.create ~tag:S.Cancel_realtime_bars ~version:1)
  ~recv_header:[Ib.Header.create ~tag:R.Realtime_bar ~version:1]
  ~tws_query:Query.Realtime_bars.pickler
  ~tws_response:[Response.Realtime_bar.unpickler]
  ()

(* ========================= TAQ data ============================= *)

let req_taq_data = Ib.Streaming_request.create
  ~send_header:(Ib.Header.create ~tag:Send_tag.Market_data ~version:9)
  ~canc_header:(Ib.Header.create ~tag:Send_tag.Cancel_market_data ~version:1)
  ~recv_header:[
    Ib.Header.create ~tag:R.Tick_price ~version:6;
    Ib.Header.create ~tag:R.Tick_size ~version:6;
  ]
  ~skip_header:[
    Ib.Header.create ~tag:R.Tick_string ~version:6;
    Ib.Header.create ~tag:R.Tick_generic ~version:6;
  ]
  ~tws_query:Query.Market_data.pickler
  ~tws_response:[
    U.map Response.Tick_price.unpickler ~f:(fun x -> `Tick_price x);
    U.map Response.Tick_size.unpickler  ~f:(fun x -> `Tick_size  x);
  ] ()

(* ========================= Snapshots ============================ *)

let req_snapshot = Ib.Streaming_request.create
  ~send_header:(Ib.Header.create ~tag:S.Market_data ~version:9)
  ~canc_header:(Ib.Header.create ~tag:S.Cancel_market_data ~version:1)
  ~recv_header:[
    Ib.Header.create ~tag:R.Tick_price ~version:6;
    Ib.Header.create ~tag:R.Snapshot_end ~version:1;
  ]
  ~skip_header:[
    Ib.Header.create ~tag:R.Tick_size ~version:6;
    Ib.Header.create ~tag:R.Tick_option ~version:6;
    Ib.Header.create ~tag:R.Tick_string ~version:6;
    Ib.Header.create ~tag:R.Tick_generic ~version:6;
  ]
  ~tws_query:Query.Market_data.pickler
  ~tws_response:[
    U.map Response.Tick_price.unpickler ~f:(fun x -> `Tick_price x);
    U.const `Snapshot_end;
  ] ()
