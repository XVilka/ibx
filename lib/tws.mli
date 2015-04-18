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

open Core.Std
open Async.Std
open Std_internal
open Response

module Query_id : Unique_id

type t (** A TWS client *)

(** {1 Connection and server} *)
(******************************************************************************)

(** [with_client ~host ~port ~on_handler_error handler] connects to the
    IB connectivity software on ([host], [port]) and runs the [handler]
    until an exception is thrown or the returned Deferred is determined.

    [on_handler_error] determines what happens if the [handler] throws an
    exception.

    The standard port for TWS is 7496 and for the IB Gateway it is 4001.
*)
val with_client
  :  ?enable_logging:bool
  -> ?client_id:Client_id.t
  -> host:string
  -> port:int
  -> on_handler_error:[
  | `Raise
  | `Ignore
  | `Call of (Error.t -> unit)
  ]
  -> (t -> unit Deferred.t)
  -> unit Deferred.t

val is_connected : t -> bool

(** [state t] returns the state of the connection. *)
val state : t -> [ `Disconnected | `Connecting | `Connected ]

(** [set_server_log_level level] sets the log entry detail [level] of TWS
    when processing API requests. *)
val set_server_log_level
  :  t
  -> level:[
  | `System
  | `Error
  | `Warning
  | `Information
  | `Detail
  ]
  -> unit

val server_time : t -> Time.t Or_error.t Deferred.t

val server_time_exn : t -> Time.t Deferred.t

val server_version : t -> int option

val connection_time : t -> Time.t option

val account_code : t -> Account_code.t option

(** {1 Market data} *)
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
  -> contract:[< Security_type.t ] Contract.t
  -> (Market_data.t Tws_result.t Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

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
  -> contract:[< Security_type.t ] Contract.t
  -> (Market_data.t Pipe.Reader.t * Query_id.t) Deferred.t

val cancel_market_data : t -> Query_id.t -> unit

val calc_option_price
  :  t
  -> contract:[ `Option ] Contract.t
  -> volatility:float
  -> underlying_price:Price.t
  -> Price.t Or_error.t Deferred.t

val calc_option_price_exn
  :  t
  -> contract:[ `Option ] Contract.t
  -> volatility:float
  -> underlying_price:Price.t
  -> Price.t Deferred.t

val calc_implied_volatility
  :  t
  -> contract:[ `Option ] Contract.t
  -> option_price:Price.t
  -> underlying_price:Price.t
  -> float Or_error.t Deferred.t

val calc_implied_volatility_exn
  :  t
  -> contract:[ `Option ] Contract.t
  -> option_price:Price.t
  -> underlying_price:Price.t
  -> float Deferred.t


(** {1 Orders} *)
(******************************************************************************)

val submit_order
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> order:([< Order.Action.t ], [< Order.Type.t ]) Order.t
  -> (Order_status.t Tws_result.t Pipe.Reader.t * Order_id.t) Or_error.t Deferred.t

val submit_order_exn
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> order:([< Order.Action.t ], [< Order.Type.t ]) Order.t
  -> (Order_status.t Pipe.Reader.t * Order_id.t) Deferred.t

val cancel_order_status : t -> Order_id.t -> unit


(** {1 Account and portfolio} *)
(******************************************************************************)

val account_updates : t -> Account_update.t Pipe.Reader.t Or_error.t Deferred.t

val account_updates_exn : t -> Account_update.t Pipe.Reader.t Deferred.t

val portfolio_positions: t -> Portfolio_position.t Pipe.Reader.t Or_error.t Deferred.t

val portfolio_positions_exn : t -> Portfolio_position.t Pipe.Reader.t Deferred.t

val commissions : t -> Commission.t Pipe.Reader.t


(** {1 Execution reports} *)
(******************************************************************************)

val executions : t -> Execution.t Pipe.Reader.t

val filter_executions
  :  ?time:Time.t
  -> t
  -> contract:[< Security_type.t ] Contract.t
  -> order_action:Order.Action.t
  -> Execution.t Tws_result.t Pipe.Reader.t Or_error.t Deferred.t

val filter_executions_exn
  :  ?time:Time.t
  -> t
  -> contract:[< Security_type.t ] Contract.t
  -> order_action:Order.Action.t
  -> Execution.t Pipe.Reader.t Deferred.t


(** {1 Contract details} *)
(******************************************************************************)

val contract_details
  :  t
  -> ?contract_id:Contract_id.t
  -> ?multiplier:string
  -> ?listing_exchange:Exchange.t
  -> ?local_symbol:Symbol.t
  -> ?security_id:Security_id.t
  -> ?include_expired:bool
  -> ?exchange:Exchange.t
  -> ?option_right:Option_right.t
  -> ?expiry:Date.t
  -> ?strike:Price.t
  -> currency:Currency.t
  -> security_type:Security_type.t
  -> Symbol.t
  -> Contract_data.t Tws_result.t Pipe.Reader.t Or_error.t Deferred.t

val contract_details_exn
  :  t
  -> ?contract_id:Contract_id.t
  -> ?multiplier:string
  -> ?listing_exchange:Exchange.t
  -> ?local_symbol:Symbol.t
  -> ?security_id:Security_id.t
  -> ?include_expired:bool
  -> ?exchange:Exchange.t
  -> ?option_right:Option_right.t
  -> ?expiry:Date.t
  -> ?strike:Price.t
  -> currency:Currency.t
  -> security_type:Security_type.t
  -> Symbol.t
  -> Contract_data.t Pipe.Reader.t Deferred.t

val futures_chain
  :  t
  -> ?contract_id:Contract_id.t
  -> ?multiplier:string
  -> ?listing_exchange:Exchange.t
  -> ?local_symbol:Symbol.t
  -> ?security_id:Security_id.t
  -> ?include_expired:bool
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> Symbol.t
  -> ([> `Futures ] Contract.t) Tws_result.t Pipe.Reader.t Or_error.t Deferred.t

val futures_chain_exn
  :  t
  -> ?contract_id:Contract_id.t
  -> ?multiplier:string
  -> ?listing_exchange:Exchange.t
  -> ?local_symbol:Symbol.t
  -> ?security_id:Security_id.t
  -> ?include_expired:bool
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> Symbol.t
  -> ([> `Futures ] Contract.t) Pipe.Reader.t Deferred.t


(** {1 Market depth} *)
(******************************************************************************)

val market_depth
  :  ?num_rows:int
  -> t
  -> contract:[< Security_type.t ] Contract.t
  -> (Book_update.t Tws_result.t Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

val market_depth_exn
  :  ?num_rows:int
  -> t
  -> contract:[< Security_type.t ] Contract.t
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
  ] (* defaults to One_day *)
  -> ?bar_span:[
  | `Sec of int
  | `Day of int
  | `Week of int
  | `Month of int
  | `Year
  ] (* defaults to Year *)
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
  -> contract:[< Security_type.t ] Contract.t
  -> Historical_data.t Tws_result.t Or_error.t Deferred.t

val historical_data_exn
  :  ?bar_size:[
  | `One_sec | `Five_secs | `Fifteen_secs | `Thirty_secs
  | `One_min | `Two_mins | `Three_mins | `Five_mins | `Fifteen_mins | `Thirty_mins
  | `One_hour
  | `One_day
  ] (* defaults to One_day *)
  -> ?bar_span:[
  | `Sec of int
  | `Day of int
  | `Week of int
  | `Month of int
  | `Year
  ] (* defaults to Year *)
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
  -> contract:[< Security_type.t ] Contract.t
  -> Historical_data.t Deferred.t


(** {1 Realtime bars} *)
(******************************************************************************)

val realtime_bars
  :  ?bar_size:[ `Five_secs ] (* TWS API supports only 5 second bars. *)
  -> ?show:[ `Trades | `Midpoint | `Bid | `Ask ]
  -> ?use_rth:bool
  -> t
  -> contract:[< Security_type.t ] Contract.t
  -> (Realtime_bar.t Tws_result.t Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

val realtime_bars_exn
  :  ?bar_size:[ `Five_secs ]
  -> ?show:[ `Trades | `Midpoint | `Bid | `Ask ]
  -> ?use_rth:bool
  -> t
  -> contract:[< Security_type.t ] Contract.t
  -> (Realtime_bar.t Pipe.Reader.t * Query_id.t) Deferred.t

val cancel_realtime_bars : t -> Query_id.t -> unit


(** {1 TAQ data} *)
(******************************************************************************)

module Trade : sig
  type t
  with sexp

  val stamp : t -> Time.t
  val price : t -> Price.t
  val size  : t -> Volume.t

  val pp : Format.formatter -> t -> unit
end

val trades
  : t
  -> contract:[< Security_type.t ] Contract.t
  -> (Trade.t Tws_result.t Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

val trades_exn
  : t
  -> contract:[< Security_type.t ] Contract.t
  -> (Trade.t Pipe.Reader.t * Query_id.t) Deferred.t

val cancel_trades : t -> Query_id.t -> unit

module Quote : sig
  type t
  with sexp

  val stamp     : t -> Time.t
  val ask_size  : t -> Volume.t
  val bid_size  : t -> Volume.t
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
  val ask_size_change  : t -> Volume.t
  val bid_size_change  : t -> Volume.t

  val pp : Format.formatter -> t -> unit
end

val quotes
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> (Quote.t Tws_result.t Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

val quotes_exn
  :  t
  -> contract:[< Security_type.t ] Contract.t
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
  -> contract:[< Security_type.t ] Contract.t
  -> (TAQ.t Tws_result.t Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

val taq_data_exn
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> (TAQ.t Pipe.Reader.t * Query_id.t) Deferred.t

val cancel_taq_data : t -> Query_id.t -> unit


(** {1 TAQ snapshots} *)
(******************************************************************************)

module Quote_snapshot : sig
  type t
  val symbol    : t -> Symbol.t
  val ask_size  : t -> Volume.t
  val bid_size  : t -> Volume.t
  val ask_price : t -> Price.t
  val bid_price : t -> Price.t
end

val quote_snapshot
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> Quote_snapshot.t Or_error.t Deferred.t

val quote_snapshot_exn
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> Quote_snapshot.t Deferred.t

module Trade_snapshot : sig
  type t
  val symbol     : t -> Symbol.t
  val last_size  : t -> Volume.t

  (** [last_price t] returns the price of the last trade or the last closing
      price if no trading is going on. *)
  val last_price : t -> Price.t
end

val trade_snapshot
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> Trade_snapshot.t Or_error.t Deferred.t

val trade_snapshot_exn
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> Trade_snapshot.t Deferred.t
