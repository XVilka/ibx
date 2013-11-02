(* File: tws.mli

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

(** A TWS client *)

open Core.Std
open Async.Std
open Std_internal
open Response

type t
include Client_intf.S with type t := t

(** {1 Connection and server} *)
(******************************************************************************)

val server_time : t -> Time.t Or_error.t Deferred.t

val server_time_exn : t -> Time.t Deferred.t


(* {1 Market data} *)
(******************************************************************************)

module Market_data : sig
  type t =
  [ `Tick_price  of Tick_price.t
  | `Tick_size   of Tick_size.t
  | `Tick_option of Tick_option.t
  | `Tick_string of Tick_string.t
  ] with sexp
  include Response_intf.Wrapper.S with type t := t
  val pp : Format.formatter -> t -> unit
end

val market_data
  :  ?snapshot:bool
  -> ?tick_generics:[
  | `Option_volume
  | `Option_open_interest
  | `Historical_volatility
  | `Implied_volatility
  | `Index_future_premium
  | `Misc_stats
  | `Mark_price
  | `Auction_values
  | `Realtime_volume
  | `Shortable
  | `Inventory
  | `Fundamental_ratios
  | `Turn_off_market_data
  ] list
  -> t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> (Market_data.t Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

val market_data_exn
  :  ?snapshot:bool
  -> ?tick_generics:[
  | `Option_volume
  | `Option_open_interest
  | `Historical_volatility
  | `Implied_volatility
  | `Index_future_premium
  | `Misc_stats
  | `Mark_price
  | `Auction_values
  | `Realtime_volume
  | `Shortable
  | `Inventory
  | `Fundamental_ratios
  | `Turn_off_market_data
  ] list
  -> t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> (Market_data.t Pipe.Reader.t * Query_id.t) Deferred.t

val cancel_market_data : t -> Query_id.t -> unit

val option_price
  :  t
  -> contract:[ `Option ] Contract.t
  -> volatility:float
  -> underlying_price:Price.t
  -> Price.t Or_error.t Deferred.t

val option_price_exn
  :  t
  -> contract:[ `Option ] Contract.t
  -> volatility:float
  -> underlying_price:Price.t
  -> Price.t Deferred.t

val implied_volatility
  :  t
  -> contract:[ `Option ] Contract.t
  -> option_price:Price.t
  -> underlying_price:Price.t
  -> float Or_error.t Deferred.t

val implied_volatility_exn
  :  t
  -> contract:[ `Option ] Contract.t
  -> option_price:Price.t
  -> underlying_price:Price.t
  -> float Deferred.t


(** {1 Orders} *)
(******************************************************************************)

val submit_order
  :  t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> order:([< `Buy | `Sell | `Sell_short ],
            [< Order.Type.t ]) Order.t
  -> (Order_status.t Pipe.Reader.t * Order_id.t) Or_error.t Deferred.t

val submit_order_exn
  :  t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> order:([< `Buy | `Sell | `Sell_short ],
            [< Order.Type.t ]) Order.t
  -> (Order_status.t Pipe.Reader.t * Order_id.t) Deferred.t

val cancel_order_status : t -> Order_id.t -> unit

(** {1 Account and portfolio} *)
(******************************************************************************)

val account_and_portfolio_updates
  :  t
  -> ([ `Account_update of Account_update.t
      | `Portfolio_update of Portfolio_update.t
      ] Pipe.Reader.t) Or_error.t Deferred.t

val account_and_portfolio_updates_exn
  :  t
  -> ([ `Account_update of Account_update.t
      | `Portfolio_update of Portfolio_update.t
      ] Pipe.Reader.t) Deferred.t

(** {1 Execution reports} *)
(******************************************************************************)

val filter_executions
  :  ?time:Time.t
  -> t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> order_action:[ `Buy | `Sell | `Sell_short ]
  -> (Execution_report.t Pipe.Reader.t) Or_error.t Deferred.t

val filter_executions_exn
  :  ?time:Time.t
  -> t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> order_action:[ `Buy | `Sell | `Sell_short ]
  -> (Execution_report.t Pipe.Reader.t) Deferred.t


(** {1 Contract specifications} *)
(******************************************************************************)

val contract_specs
  :  t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> Contract_specs.t Or_error.t Deferred.t

val contract_specs_exn
  :  t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> Contract_specs.t Deferred.t


(** {1 Market depth} *)
(******************************************************************************)

val market_depth
  :  ?num_rows:int
  -> t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> (Book_update.t Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

val market_depth_exn
  :  ?num_rows:int
  -> t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> (Book_update.t Pipe.Reader.t * Query_id.t) Deferred.t

val cancel_market_depth : t -> Query_id.t -> unit


(** {1 Historical data} *)
(******************************************************************************)

val historical_data
  :  ?bar_size:[
  | `One_sec | `Five_secs | `Fifteen_secs | `Thirty_secs
  | `One_min | `Two_mins | `Three_mins | `Five_mins | `Fifteen_mins | `Thirty_mins
  | `One_hour
  | `One_day
  ]
  -> ?duration:[
  | `Sec of int
  | `Day of int
  | `Week of int
  | `Month of int
  | `Year
  ]
  -> ?use_rth:bool
  -> ?show:[
  | `Trades
  | `Midpoint
  | `Bid
  | `Ask
  | `Bid_ask
  | `Historical_volatility
  | `Implied_volatility
  | `Option_volume
  ]
  -> ?until:Time.t
  -> t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> Historical_data.t Or_error.t Deferred.t

val historical_data_exn
  :  ?bar_size:[
  | `One_sec | `Five_secs | `Fifteen_secs | `Thirty_secs
  | `One_min | `Two_mins | `Three_mins | `Five_mins | `Fifteen_mins | `Thirty_mins
  | `One_hour
  | `One_day
  ]
  -> ?duration:[
  | `Sec of int
  | `Day of int
  | `Week of int
  | `Month of int
  | `Year
  ]
  -> ?use_rth:bool
  -> ?show:[
  | `Trades
  | `Midpoint
  | `Bid
  | `Ask
  | `Bid_ask
  | `Historical_volatility
  | `Implied_volatility
  | `Option_volume
  ]
  -> ?until:Time.t
  -> t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> Historical_data.t Deferred.t


(** {1 Realtime bars} *)
(******************************************************************************)

val realtime_bars
  :  ?bar_size:[ `Five_secs ] (* TWS API supports only 5 second bars. *)
  -> ?show:[ `Trades | `Midpoint | `Bid | `Ask ]
  -> ?use_rth:bool
  -> t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> (Realtime_bar.t Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

val realtime_bars_exn
  :  ?bar_size:[ `Five_secs ]
  -> ?show:[ `Trades | `Midpoint | `Bid | `Ask ]
  -> ?use_rth:bool
  -> t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> (Realtime_bar.t Pipe.Reader.t * Query_id.t) Deferred.t

val cancel_realtime_bars : t -> Query_id.t -> unit


(** {1 TAQ data} *)
(******************************************************************************)

module Trade : sig
  type t
  with sexp

  val time  : t -> Time.t
  val price : t -> Price.t
  val size  : t -> int

  val pp : Format.formatter -> t -> unit
end

val trades
  : t
  -> contract:[< `Stock | `Futures | `Option ] Contract.t
  -> (Trade.t Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

val trades_exn
  : t
  -> contract:[< `Stock | `Futures | `Option ] Contract.t
  -> (Trade.t Pipe.Reader.t * Query_id.t) Deferred.t

val cancel_trades : t -> Query_id.t -> unit

module Quote : sig
  type t
  with sexp

  val time      : t -> Time.t
  val ask_size  : t -> int
  val bid_size  : t -> int
  val ask_price : t -> Price.t
  val bid_price : t -> Price.t
  val change    : t ->
    [ `Unknown
    | `Ask_size_change
    | `Bid_size_change
    | `Ask_price_change
    | `Bid_price_change
    | `Ask_size_and_price_change
    | `Bid_size_and_price_change ]
  val ask_price_change : t -> Price.t
  val bid_price_change : t -> Price.t
  val ask_size_change  : t -> int
  val bid_size_change  : t -> int

  val pp : Format.formatter -> t -> unit
end

val quotes
  :  t
  -> contract:[< `Stock | `Futures | `Option ] Contract.t
  -> (Quote.t Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

val quotes_exn
  :  t
  -> contract:[< `Stock | `Futures | `Option ] Contract.t
  -> (Quote.t Pipe.Reader.t * Query_id.t) Deferred.t

val cancel_quotes : t -> Query_id.t -> unit

module TAQ : sig
  type t =
  | Trade of Trade.t
  | Quote of Quote.t
  with sexp

  val pp : Format.formatter -> t -> unit
end

val taq_data
  :  t
  -> contract:[< `Stock | `Futures | `Option ] Contract.t
  -> (TAQ.t Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

val taq_data_exn
  :  t
  -> contract:[< `Stock | `Futures | `Option ] Contract.t
  -> (TAQ.t Pipe.Reader.t * Query_id.t) Deferred.t

val cancel_taq_data : t -> Query_id.t -> unit


(** {1 TAQ snapshots} *)
(******************************************************************************)

module Quote_snapshot : sig
  type t
  val symbol    : t -> Symbol.t
  val ask_size  : t -> int
  val bid_size  : t -> int
  val ask_price : t -> Price.t
  val bid_price : t -> Price.t
end

val quote_snapshot
  :  t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> Quote_snapshot.t Or_error.t Deferred.t

val quote_snapshot_exn
  :  t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> Quote_snapshot.t Deferred.t

module Trade_snapshot : sig
  type t
  val symbol     : t -> Symbol.t
  val last_size  : t -> int
  val last_price : t -> Price.t
end

val trade_snapshot
  :  t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> Trade_snapshot.t Or_error.t Deferred.t

val trade_snapshot_exn
  :  t
  -> contract:[< `Stock | `Futures | `Option | `Forex ] Contract.t
  -> Trade_snapshot.t Deferred.t
