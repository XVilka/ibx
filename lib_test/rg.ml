(* File: rg.ml

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
open Ibx.Std

module Rg_common : sig

  type 'a gen = unit -> 'a

  val bg : bool gen
  val nng : int gen
  val sg : string gen
  val pfg : float gen
  val tmg : Time.t gen
  val tzg : Time.Zone.t gen

  val always : 'a -> 'a gen
  val og : 'a gen -> 'a option gen
  val oneof : 'a gen list -> 'a gen

  val query_id_g : Query_id.t gen
  val client_id_g : Client_id.t gen
  val order_id_g : Order_id.t gen
  val contract_id_g : Contract_id.t gen

  val version_g : int gen
  val account_code_g : Account_code.t gen

  val currency_g : Currency.t gen
  val price_g : Price.t gen
  val volume_g : Volume.t gen
  val symbol_g : Symbol.t gen
  val exchange_g : Exchange.t gen
  val expiry_g : Date.t gen
  val option_right_g : [ `Call | `Put ] gen
  val security_id_g :  [ `ISIN  of string
                       | `RIC   of string
                       | `CUSIP of string
                       | `SEDOL of string ] gen
  val security_type_g : Security_type.t gen

  val option_g : [ `Option ] Contract.t gen
  val contract_g : Security_type.t Contract.t gen

  val order_action_g : Order.Action.t gen
  val order_g : (Order.Action.t, Order.Type.t) Order.t gen

end = struct

  type 'a gen = unit -> 'a

  let bg = Random.bool

  let nng () =
    let p = Random.float 1. in
    if p < 0.5 then Random.int 10
    else if p < 0.95 then Random.int 100
    else Random.int 1_000

  let sg () =
    let cg () = Char.of_int_exn (97 + Random.int 26) in
    let size = 1 + nng () in
    let s = String.create size in
    for i = 0 to String.length s - 1 do
      s.[i] <- cg ()
    done;
    s

  let pfg () = Random.float 100. +. 20.

  let tmg () =
    Time.now ()
    |> Time.to_float
    |> Float.modf
    |> Float.Parts.integral
    |> Time.of_float

  let tzg () = Time.Zone.local

  let always x () = x

  let og g () =
    if bg () then Some (g ()) else None

  let oneof xs =
    List.nth_exn xs (Random.int (List.length xs))

  let query_id_g    () = Query_id.create ()
  let client_id_g   () = Client_id.create ()
  let order_id_g    () = Order_id.create ()
  let contract_id_g () = Contract_id.create ()

  let version_g () = 1 + Random.int 30
  let account_code_g () = Account_code.of_string (sg ())

  let currency_g () = oneof [
     always (`USD);
     always (`AUD);
     always (`CAD);
     always (`CHF);
     always (`CNH);
     always (`DKK);
     always (`EUR);
     always (`GBP);
     always (`HKD);
     always (`HUF);
     always (`ILS);
     always (`JPY);
     always (`MXN);
     always (`NOK);
     always (`NZD);
     always (`RUB);
     always (`SEK);
     always (`SGD);
     always (`KRW);
  ] ()

  let price_g  () = Price.of_float (pfg ())
  let volume_g () = Volume.of_int_exn (nng ())
  let symbol_g () = Symbol.of_string (sg ())

  let exchange_g () = oneof [
    always (`SMART);
    always (`ARCAEDGE);
    always (`BYX);
    always (`BATS);
    always (`BTRADE);
    always (`BOX);
    always (`CFE);
    always (`CBSX);
    always (`ECBOT);
    always (`CBOE);
    always (`CBOE2);
    always (`CHX);
    always (`GLOBEX);
    always (`DRCTEDGE);
    always (`EDGEA);
    always (`ELX);
    always (`ICEUS);
    always (`ISLAND);
    always (`ISE);
    always (`LAVA);
    always (`NASDAQ);
    always (`NFX);
    always (`NASDAQOM);
    always (`BEX);
    always (`PSX);
    always (`NSX);
    always (`NYBOT);
    always (`NYMEX);
    always (`NYSE);
    always (`AMEX);
    always (`ARCA);
    always (`LIFFE);
    always (`ONE);
    always (`PSE);
    always (`PHLX);
    always (`PINK);
    always (`ALPHA);
    always (`CDE);
    always (`OMEGA);
    always (`PURE);
    always (`SELECT);
    always (`TSE);
    always (`VENTURE);
    always (`MEXDER);
    always (`MEXI);
    always (`VSE);
    always (`BATEEN);
    always (`CHIXEN);
    always (`SBVM);
    always (`TRQXEN);
    always (`MATIF);
    always (`MONEP);
    always (`SBF);
    always (`BATEDE);
    always (`CHIXDE);
    always (`DTB);
    always (`FWB);
    always (`SWB);
    always (`TRADEGATE);
    always (`TRQXDE);
    always (`IBIS);
    always (`BVME);
    always (`IDEM);
    always (`IBCFD);
    always (`FTA);
    always (`AEB);
    always (`BM);
    always (`MEFF);
    always (`OMS);
    always (`SFB);
    always (`BATECH);
    always (`CHIXCH);
    always (`SOFFEX);
    always (`SWX);
    always (`TRQXCH);
    always (`VIRTX);
    always (`BATEUK);
    always (`CHIXUK);
    always (`LSE);
    always (`ASX);
    always (`SNFE);
    always (`HKFE);
    always (`HKMEX);
    always (`SEHK);
    always (`IBSX);
    always (`CHIXJ);
    always (`TSEJ);
    always (`SGX);
    always (`KSE);
    always (`IDEAL);
    always (`IDEALPRO);
    always (`MIBSX);
  ] ()

  let expiry_g   () = Date.of_tm (Core.Std.Unix.localtime (Core.Std.Unix.time ()))

  let option_right_g () = oneof [
    always (`Call);
    always (`Put);
  ] ()

  let security_id_g () = oneof [
    always (`ISIN  (sg ()));
    always (`CUSIP (sg ()));
    always (`SEDOL (sg ()));
    always (`RIC   (sg ()));
  ] ()

  let security_type_g () =
    let open Security_type in
    oneof [
      always (`Stock);
      always (`Futures);
      always (`Option);
      always (`Fut_opt);
      always (`Forex);
    ] ()

  let option_g () =
    Contract.option
      ~multiplier:(nng ())
      ~local_symbol:(symbol_g ())
      ~sec_id:(security_id_g ())
      ~exchange:(exchange_g ())
      ~currency:(currency_g ())
      ~strike:(price_g ())
      ~option_right:(option_right_g ())
      ~expiry:(expiry_g ())
      (symbol_g ())

  let contract_g () = oneof
    [ always (
      Contract.stock
        ?id:(og contract_id_g ())
        ?listed_on:(og exchange_g ())
        ?local_symbol:(og symbol_g ())
        ?sec_id:(og security_id_g ())
        ?exchange:(og exchange_g ())
        ~currency:(currency_g ())
        (symbol_g ()))
    ; always (
      Contract.futures
        ?id:(og contract_id_g ())
        ?multiplier:(og nng ())
        ?local_symbol:(og symbol_g ())
        ?sec_id:(og security_id_g ())
        ?include_expired:(og bg ())
        ?exchange:(og exchange_g ())
        ~currency:(currency_g ())
        ~expiry:(expiry_g ())
        (symbol_g ()))
    ; always (
      Contract.option
        ?id:(og contract_id_g ())
        ?multiplier:(og nng ())
        ?local_symbol:(og symbol_g ())
        ?sec_id:(og security_id_g ())
        ?exchange:(og exchange_g ())
        ~currency:(currency_g ())
        ~option_right:(option_right_g ())
        ~expiry:(expiry_g ())
        ~strike:(price_g ())
        (symbol_g ()))
    ] ()

  let order_action_g () = oneof [
    always (`Buy);
    always (`Sell);
    always (`Sell_short);
  ] ()

  let order_g () = oneof [
    always (Order.buy_market  ~quantity:(volume_g ()));
    always (Order.sell_market ~quantity:(volume_g ()));
    always (Order.buy_limit   ~quantity:(volume_g ()) (price_g ()));
    always (Order.buy_limit   ~quantity:(volume_g ()) (price_g ()));
  ] ()

end
include Rg_common

module Q : sig

  (* Connection and server *)

  val server_log_level_g : Query.Server_log_level.t gen
  val server_time_g : Query.Server_time.t gen

  (* Market data *)

  val market_data_g : Query.Market_data.t gen
  val calc_option_price_g : Query.Calc_option_price.t gen
  val calc_implied_volatility_g : Query.Calc_implied_volatility.t gen

  (* Orders *)

  val submit_order_g : Query.Submit_order.t gen

  (* Account and portfolio *)

  val account_updates : Query.Account_updates.t gen
  val positions : Query.Positions.t gen

  (* Executions *)

  val executions_g : Query.Executions.t gen

  (* Contract details *)

  val contract_details_g : Query.Contract_details.t gen

  (* Market depth *)

  val market_depth_g : Query.Market_depth.t gen

  (* Historical data *)

  val historical_data_g : Query.Historical_data.t gen

  (* Realtime bars *)

  val realtime_bars_g : Query.Realtime_bars.t gen

end = struct

  (* ========================= Connection and server ======================= *)

  let server_log_level_g () =
    let level_g () = oneof [
      always (`System);
      always (`Error);
      always (`Warning);
      always (`Information);
      always (`Detail);
    ] ()
    in
    Query.Server_log_level.create ~level:(level_g ())

  let server_time_g () = Query.Server_time.create ()

  (* ============================== Market data ============================ *)

  let market_data_g () =
    let contract_g = oneof
      [ always (
        Contract.stock
          ?id:(og contract_id_g ())
          ?listed_on:(og exchange_g ())
          ?local_symbol:(og symbol_g ())
          ?sec_id:None
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          (symbol_g ()))
      ; always (
        Contract.futures
          ?id:(og contract_id_g ())
          ?multiplier:(og nng ())
          ?local_symbol:(og symbol_g ())
          ?sec_id:None
          ?include_expired:None
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          ~expiry:(expiry_g ())
          (symbol_g ()))
      ; always (
        Contract.option
          ?id:(og contract_id_g ())
          ?multiplier:(og nng ())
          ?local_symbol:(og symbol_g ())
          ?sec_id:None
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          ~option_right:(option_right_g ())
          ~expiry:(expiry_g ())
          ~strike:(price_g ())
          (symbol_g ()))
      ]
    in
    let tick_generics_g = oneof [
      always ([]);
      always ([
        `Option_volume;
        `Option_open_interest;
        `Historical_volatility;
        `Implied_volatility;
        `Index_future_premium;
        `Misc_stats;
        `Mark_price;
        `Auction_values;
        `Realtime_volume;
        `Shortable;
        `Inventory;
        `Fundamental_ratios;
        `Turn_off_market_data;
      ])
    ] in
    Query.Market_data.create
      ~contract:(contract_g ())
      ~tick_generics:(tick_generics_g ())
      ~snapshot:(bg ())

  let calc_option_price_g () =
    let option_g () =
      Contract.option
        ~multiplier:(nng ())
        ~local_symbol:(symbol_g ())
        ?sec_id:None
        ~exchange:(exchange_g ())
        ~currency:(currency_g ())
        ~strike:(price_g ())
        ~option_right:(option_right_g ())
        ~expiry:(expiry_g ())
        (symbol_g ())
    in
    Query.Calc_option_price.create
      ~contract:(option_g ())
      ~volatility:(pfg ())
      ~underlying_price:(price_g ())

  let calc_implied_volatility_g () =
    let option_g () =
      Contract.option
        ~multiplier:(nng ())
        ~local_symbol:(symbol_g ())
        ?sec_id:None
        ~exchange:(exchange_g ())
        ~currency:(currency_g ())
        ~strike:(price_g ())
        ~option_right:(option_right_g ())
        ~expiry:(expiry_g ())
        (symbol_g ())
    in
    Query.Calc_implied_volatility.create
      ~contract:(option_g ())
      ~option_price:(price_g ())
      ~underlying_price:(price_g ())

  (* ============================== Orders ================================= *)

  let submit_order_g () =
    let contract_g () = oneof
      [ always (
        Contract.stock
          ?id:(og contract_id_g ())
          ?listed_on:(og exchange_g ())
          ?local_symbol:(og symbol_g ())
          ?sec_id:(og security_id_g ())
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          (symbol_g ()))
      ; always (
        Contract.futures
          ?id:(og contract_id_g ())
          ?multiplier:(og nng ())
          ?local_symbol:(og symbol_g ())
          ?sec_id:(og security_id_g ())
          ?include_expired:(og bg ())
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          ~expiry:(expiry_g ())
          (symbol_g ()))
      ; always (
        Contract.option
          ?id:(og contract_id_g ())
          ?multiplier:(og nng ())
          ?local_symbol:(og symbol_g ())
          ?sec_id:(og security_id_g ())
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          ~option_right:(option_right_g ())
          ~expiry:(expiry_g ())
          ~strike:(price_g ())
          (symbol_g ()))
      ] ()
    in
    Query.Submit_order.create
      ~contract:(contract_g ())
      ~order:(order_g ())
      ~account_code:(account_code_g ())

  (* =================== Account and portfolio updates ===================== *)

  let account_updates () =
    Query.Account_updates.create
      ~subscribe:(bg ())
      ~account_code:(account_code_g ())

  let positions () =
    Query.Positions.create
      ~subscribe:(bg ())
      ~account_code:(account_code_g ())

  (* ============================= Executions ============================== *)

  let executions_g () =
    Query.Executions.create
      ~contract:(contract_g ())
      ~client_id:(client_id_g ())
      ~account_code:(account_code_g ())
      ~time:(tmg ())
      ~order_action:(order_action_g ())

  (* ========================== Contract details =========================== *)

  let contract_details_g () =
    Query.Contract_details.create
      ?contract_id:(og contract_id_g ())
      ?multiplier:(og nng ())
      ?local_symbol:(og symbol_g ())
      ?sec_id:(og security_id_g ())
      ?exchange:(og exchange_g ())
      ~currency:(currency_g ())
      ~option_right:(option_right_g ())
      ~expiry:(expiry_g ())
      ~strike:(price_g ())
      ~sec_type:(security_type_g ())
      (symbol_g ())

  (* ============================ Market depth ============================= *)

  let market_depth_g () =
    let contract_g = oneof
      [ always (
        Contract.stock
          ~local_symbol:(symbol_g ())
          ~exchange:(exchange_g ())
          ~currency:(currency_g ())
          (symbol_g ()))
      ; always (
        Contract.futures
          ~multiplier:(nng ())
          ~local_symbol:(symbol_g ())
          ~exchange:(exchange_g ())
          ~currency:(currency_g ())
          ~expiry:(expiry_g ())
          (symbol_g ()))
      ; always (
        Contract.option
          ~multiplier:(nng ())
          ~local_symbol:(symbol_g ())
          ~exchange:(exchange_g ())
          ~currency:(currency_g ())
          ~option_right:(option_right_g ())
          ~expiry:(expiry_g ())
          ~strike:(price_g ())
          (symbol_g ()))
      ]
    in
    Query.Market_depth.create
      ~contract:(contract_g ())
      ~num_rows:(nng ())

  (* =========================== Historical data =========================== *)

  let historical_data_g () =
    let contract_g () = oneof
      [ always (
        Contract.stock
          ?id:None
          ?listed_on:(og exchange_g ())
          ?local_symbol:(og symbol_g ())
          ?sec_id:None
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          (symbol_g ()))
      ; always (
        Contract.futures
          ?id:None
          ?multiplier:(og nng ())
          ?local_symbol:(og symbol_g ())
          ?sec_id:None
          ?include_expired:(og bg ())
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          ~expiry:(expiry_g ())
          (symbol_g ()))
      ; always (
        Contract.option
          ?id:None
          ?multiplier:(og nng ())
          ?local_symbol:(og symbol_g ())
          ?sec_id:None
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          ~option_right:(option_right_g ())
          ~expiry:(expiry_g ())
          ~strike:(price_g ())
          (symbol_g ()))
      ] ()
    in
    let bar_size_g () = oneof [
      always (`One_sec);
      always (`Five_secs);
      always (`Fifteen_secs);
      always (`Thirty_secs);
      always (`One_min);
      always (`Two_mins);
      always (`Three_mins);
      always (`Five_mins);
      always (`Fifteen_mins);
      always (`Thirty_mins);
      always (`One_hour);
      always (`One_day);
    ] ()
    in
    let bar_span_g () = oneof [
      always (`Sec (1 + nng ()));
      always (`Day (1 + nng ()));
      always (`Week (1 + nng ()));
      always (`Month (1 + nng ()));
      always (`Year (1 + nng ()));
    ] ()
    in
    let show_g () = oneof [
      always (`Trades);
      always (`Midpoint);
      always (`Bid);
      always (`Ask);
      always (`Bid_ask);
      always (`Historical_volatility);
      always (`Implied_volatility);
      always (`Option_volume);
    ] ()
    in
    Query.Historical_data.create
      ~contract:(contract_g ())
      ~until:(tmg ())
      ~bar_size:(bar_size_g ())
      ~bar_span:(bar_span_g ())
      ~use_rth:(bg ())
      ~show:(show_g ())

  (* =========================== Realtime bars ============================= *)

  let realtime_bars_g () =
    let contract_g () = oneof
      [ always (
        Contract.stock
          ?id:None
          ?listed_on:(og exchange_g ())
          ?local_symbol:(og symbol_g ())
          ?sec_id:None
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          (symbol_g ()))
      ; always (
        Contract.futures
          ?id:None
          ?multiplier:(og nng ())
          ?local_symbol:(og symbol_g ())
          ?sec_id:None
          ?include_expired:None
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          ~expiry:(expiry_g ())
          (symbol_g ()))
      ; always (
        Contract.option
          ?id:None
          ?multiplier:(og nng ())
          ?local_symbol:(og symbol_g ())
          ?sec_id:None
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          ~option_right:(option_right_g ())
          ~expiry:(expiry_g ())
          ~strike:(price_g ())
          (symbol_g ()))
      ] ()
    in
    let show_g () = oneof [
      always (`Trades);
      always (`Midpoint);
      always (`Bid);
      always (`Ask);
    ] ()
    in
    Query.Realtime_bars.create
      ~contract:(contract_g ())
      ~bar_size:`Five_secs
      ~show:(show_g ())
      ~use_rth:(bg ())
end

module R : sig

  (* Connection and Server *)

  val tws_error_g   : Response.Tws_error.t gen
  val server_time_g : Response.Server_time.t gen

  (* Market data *)

  val tick_price_g  : Response.Tick_price.t gen
  val tick_size_g   : Response.Tick_size.t gen
  val tick_option_g : Response.Tick_option.t gen
  val tick_string_g : Response.Tick_string.t gen
  (* val efp_tick_g : Response.Tick_efp.t gen *)

  val market_data_g : Tws.Market_data.t list gen

  (* Orders *)

  val order_status_g : Response.Order_status.t gen
  val order_states_g : Response.Order_status.t list gen

  (* Account and Portfolio *)

  val account_update_g  : Response.Account_update.t gen
  val account_updates_g : Response.Account_update.t list gen

  val position_g  : Response.Position.t gen
  val positions_g : Response.Position.t list gen

  (* Contract details *)

  val contract_data_g    : Response.Contract_data.t gen
  val contract_details_g : Response.Contract_data.t list gen

  (* Executions *)

  val execution_g  : Response.Execution.t gen
  val executions_g : Response.Execution.t list gen

  (* Market depth *)

  val book_update_g  : Response.Book_update.t gen
  val book_updates_g : Response.Book_update.t list gen

  (* Historical data *)

  val historical_data_g : Response.Historical_data.t gen

  (* Realtime bars *)

  val realtime_bar_g  : Response.Realtime_bar.t gen
  val realtime_bars_g : Response.Realtime_bar.t list gen

end = struct

  let bound = 1_000

  (* ========================= Connection and server ======================= *)

  let tws_error_g () =
    Response.Tws_error.create
      ~error_code:(nng ())
      ~error_msg:(sg ())

  let server_time_g () = Response.Server_time.create ~time:(tmg ())

  (* ============================== Market data ============================ *)

  let tick_price_g () =
    let module Type = Response.Tick_price.Type in
    let tick_type_g () = oneof [
      always Type.Bid;
      always Type.Ask;
      always Type.Last;
      always Type.High;
      always Type.Low;
      always Type.Close;
    ] ()
    in
    Response.Tick_price.create
      ~tick_type:(tick_type_g ())
      ~price:(price_g ())
      ~size:(volume_g ())
      ~can_auto_execute:(og bg ())

  let tick_size_g () =
    let module Type = Tick_size.Type in
    let tick_type_g () = oneof [
      always Type.Bid;
      always Type.Ask;
      always Type.Last;
      always Type.Volume;
    ] ()
    in
    Response.Tick_size.create
      ~tick_type:(tick_type_g ())
      ~size:(volume_g ())

  let tick_option_g () =
    let module Type = Tick_option.Type in
    let greeks_g () = if bg () then Random.float 1. else Random.float (-1.) in
    let tick_type_g () = oneof [
      always Type.Bid;
      always Type.Ask;
      always Type.Last;
      always Type.Model;
      always Type.Custom;
    ] ()
    in
    Response.Tick_option.create
      ~tick_type:(tick_type_g ())
      ~implied_vol:(pfg ())
      ~delta:(greeks_g ())
      ~option_price:(price_g ())
      ~pv_dividend:(pfg ())
      ~gamma:(greeks_g ())
      ~vega:(greeks_g ())
      ~theta:(greeks_g ())
      ~under_price:(price_g ())

  let tick_string_g () =
    let module Type = Response.Tick_string.Type in
    let tick_type_g () = oneof [
      always Type.Bid_size;
      always Type.Bid_price;
      always Type.Ask_price;
      always Type.Ask_size;
      always Type.Last_price;
      always Type.Last_size;
      always Type.High_price;
      always Type.Low_price;
      always Type.Volume;
      always Type.Close_price;
      always Type.Bid_option;
      always Type.Ask_option;
      always Type.Last_option;
      always Type.Model_option;
      always Type.Open_price;
      always Type.Low_13_week;
      always Type.High_13_week;
      always Type.Low_26_week;
      always Type.High_26_week;
      always Type.Low_52_week;
      always Type.High_52_week;
      always Type.Avg_volume;
      always Type.Open_interest;
      always Type.Historical_volatility;
      always Type.Implied_volatility;
      always Type.Option_bid_exch;
      always Type.Option_ask_exch;
      always Type.Call_open_interest;
      always Type.Put_open_interest;
      always Type.Call_volume;
      always Type.Put_volume;
      always Type.Index_future_premium;
      always Type.Bid_exch;
      always Type.Ask_exch;
      always Type.Auction_volume;
      always Type.Auction_price;
      always Type.Auction_imbalance;
      always Type.Mark_price;
      always Type.Bid_efp;
      always Type.Ask_efp;
      always Type.Last_efp;
      always Type.Open_efp;
      always Type.High_efp;
      always Type.Low_efp;
      always Type.Close_efp;
      always Type.Last_timestamp;
      always Type.Shortable;
      always Type.Fundamental_ratios;
      always Type.Realtime_volume;
      always Type.Halted;
      always Type.Bid_yield;
      always Type.Ask_yield;
      always Type.Last_yield;
      always Type.Cust_option_comp;
    ] ()
    in
    Response.Tick_string.create
      ~tick_type:(tick_type_g ())
      ~value:(sg ())

  let market_data_g () =
    List.permute ~random_state:(Random.State.make_self_init ())
      (List.concat
         [ List.init (1 + Random.int bound) ~f:(fun _ ->
           `Tick_price (tick_price_g ()))
         ; List.init (1 + Random.int bound) ~f:(fun _ ->
           `Tick_size (tick_size_g ()))
         ; List.init (1 + Random.int bound) ~f:(fun _ ->
           `Tick_option (tick_option_g ()))
         ; List.init (1 + Random.int bound) ~f:(fun _ ->
           `Tick_string (tick_string_g ()))
         ])

  (* ============================== Orders ================================= *)

  let order_status_g () =
    let state_g () = oneof [
      always (`Pending_submit);
      always (`Pending_cancel);
      always (`Pre_submitted);
      always (`Submitted);
      always (`Cancelled);
      always (`Filled);
      always (`Inactive);
    ] ()
    in
    Response.Order_status.create
      ~state:(state_g ())
      ~filled:(volume_g ())
      ~remaining:(volume_g ())
      ~avg_fill_price:(price_g ())
      ~permanent_id:(nng ())
      ~parent_id:(order_id_g ())
      ~last_fill_price:(price_g ())
      ~client_id:(client_id_g ())
      ~why_held:(og sg ())

  let order_states_g () =
    List.permute ~random_state:(Random.State.make_self_init ())
      (List.init (1 + Random.int bound) ~f:(fun _ -> order_status_g ()))

  (* ======================== Account and Portfolio ======================== *)

  let account_update_g () =
    Response.Account_update.create
      ~key:(sg ())
      ~value:(sg ())
      ~currency:(og sg ())
      ~account_code:(account_code_g ())

  let account_updates_g () =
    List.permute ~random_state:(Random.State.make_self_init ())
      (List.init (1 + Random.int bound) ~f:(fun _ -> account_update_g ()))

  let position_g () =
    let contract_g () = oneof
      [ always (
        Contract.stock
          ?id:(og contract_id_g ())
          ?listed_on:None
          ?local_symbol:(og symbol_g ())
          ?sec_id:None
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          (symbol_g ()))
      ; always (
        Contract.futures
          ?id:(og contract_id_g ())
          ?multiplier:(og nng ())
          ?local_symbol:(og symbol_g ())
          ?sec_id:None
          ?include_expired:None
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          ~expiry:(expiry_g ())
          (symbol_g ()))
      ; always (
        Contract.option
          ?id:(og contract_id_g ())
          ?multiplier:(og nng ())
          ?local_symbol:(og symbol_g ())
          ?sec_id:None
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          ~option_right:(option_right_g ())
          ~expiry:(expiry_g ())
          ~strike:(price_g ())
          (symbol_g ()))
      ] ()
    in
    Response.Position.create
      ~contract:(contract_g ())
      ~volume:(volume_g ())
      ~market_price:(price_g ())
      ~market_value:(price_g ())
      ~average_cost:(price_g ())
      ~unrealized_pnl:(price_g ())
      ~realized_pnl:(price_g ())
      ~account_code:(account_code_g ())

  let positions_g () =
    List.permute ~random_state:(Random.State.make_self_init ())
      (List.init (1 + Random.int bound) ~f:(fun _ -> position_g ()))

  (* =========================== Contract details ========================== *)

  let contract_data_g () =
    let contract_g () = oneof
      [ always (
        Contract.stock
          ?id:(og contract_id_g ())
          ?listed_on:(og exchange_g ())
          ?local_symbol:(og symbol_g ())
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          (symbol_g ()))
      ; always (
        Contract.futures
          ?id:(og contract_id_g ())
          ?multiplier:(og nng ())
          ?local_symbol:(og symbol_g ())
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          ~expiry:(expiry_g ())
          (symbol_g ()))
      ; always (
        Contract.option
          ?id:(og contract_id_g ())
          ?multiplier:(og nng ())
          ?local_symbol:(og symbol_g ())
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          ~option_right:(option_right_g ())
          ~expiry:(expiry_g ())
          ~strike:(price_g ())
          (symbol_g ()))
      ] ()
    in
    Response.Contract_data.create
      ~contract:(contract_g ())
      ~market_name:(sg ())
      ~trading_class:(sg ())
      ~min_tick:(pfg ())
      ~order_types:(List.init (Random.int 10) ~f:(fun _ -> sg ()))
      ~valid_exchanges:(List.init (Random.int 10) ~f:(fun _ -> exchange_g ()))
      ~price_magnifier:(nng ())
      ~underlying_id:(nng ())
      ~long_name:(sg ())
      ~contract_month:(sg ())
      ~industry:(sg ())
      ~category:(sg ())
      ~subcategory:(sg ())
      ~time_zone:(tzg ())
      ~trading_hours:(sg ())
      ~liquid_hours:(sg ())

  let contract_details_g () =
    List.permute ~random_state:(Random.State.make_self_init ())
      (List.init (1 + Random.int bound) ~f:(fun _ -> contract_data_g ()))

  (* ============================= Executions ============================== *)

  let execution_g () =
    let contract_g () = oneof
      [ always (
        Contract.stock
          ?id:(og contract_id_g ())
          ?listed_on:None
          ?local_symbol:(og symbol_g ())
          ?sec_id:None
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          (symbol_g ()))
      ; always (
        Contract.futures
          ?id:(og contract_id_g ())
          ?multiplier:None
          ?local_symbol:(og symbol_g ())
          ?sec_id:None
          ?include_expired:None
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          ~expiry:(expiry_g ())
          (symbol_g ()))
      ; always (
        Contract.option
          ?id:(og contract_id_g ())
          ?multiplier:None
          ?local_symbol:(og symbol_g ())
          ?sec_id:None
          ?exchange:(og exchange_g ())
          ~currency:(currency_g ())
          ~option_right:(option_right_g ())
          ~expiry:(expiry_g ())
          ~strike:(price_g ())
          (symbol_g ()))
      ] ()
    in
    let exec_id_g = always (Execution_id.of_string (sg ())) in
    let side_g () = oneof [
      always (`bought);
      always (`sold);
    ] ()
    in
    Response.Execution.create
      ~order_id:(order_id_g ())
      ~contract:(contract_g ())
      ~exec_id:(exec_id_g ())
      ~time:(tmg ())
      ~account_code:(account_code_g ())
      ~exchange:(exchange_g ())
      ~side:(side_g ())
      ~volume:(volume_g ())
      ~permanent_id:(nng ())
      ~price:(price_g ())
      ~client_id:(client_id_g ())
      ~liquidation:(nng ())
      ~cumulative_volume:(volume_g ())
      ~average_price:(price_g ())
      ~order_ref:(og sg ())


  let executions_g () =
    List.permute ~random_state:(Random.State.make_self_init ())
      (List.init (1 + Random.int bound) ~f:(fun _ -> execution_g ()))

  (* ============================ Market depth ============================= *)

  let book_update_g () =
    let module Operation = Book_update.Operation in
    let module Side = Book_update.Side in
    let operation_g () = oneof [
      always Operation.Insert;
      always Operation.Update;
      always Operation.Delete;
    ] ()
    in
    let side_g () = oneof [
      always Side.Bid;
      always Side.Ask;
    ] ()
    in
    Response.Book_update.create
      ~position:(nng ())
      ~operation:(operation_g ())
      ~side:(side_g ())
      ~price:(price_g ())
      ~size:(volume_g ())

  let book_updates_g () =
    List.init (1 + Random.int bound) ~f:(fun _ -> book_update_g ())

  (* =========================== Historical data =========================== *)

  let historical_data_g () =
    let bar_g () =
      Response.Historical_bar.create
        ~stamp:(tmg ())
        ~op:(price_g ())
        ~hi:(price_g ())
        ~lo:(price_g ())
        ~cl:(price_g ())
        ~vo:(volume_g ())
        ~wap:(price_g ())
        ~has_gaps:(bg ())
        ~count:(nng ())
    in
    Response.Historical_data.create
      ~start:(tmg ())
      ~stop:(tmg ())
      ~bars:(List.init (1 + Random.int bound) ~f:(fun _ -> bar_g ()))

  (* =========================== Realtime bars ============================= *)

  let realtime_bar_g () =
    Response.Realtime_bar.create
      ~stamp:(tmg ())
      ~op:(price_g ())
      ~hi:(price_g ())
      ~lo:(price_g ())
      ~cl:(price_g ())
      ~vo:(volume_g ())
      ~wap:(price_g ())
      ~count:(nng ())

  let realtime_bars_g () =
    List.init (1 + Random.int bound) ~f:(fun _ -> realtime_bar_g ())

end
