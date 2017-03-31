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

open Core
open Async
open Std_internal

module Query_id : Unique_id

type t (** A TWS client *)

(** {1 Connection and server} *)
(******************************************************************************)

(** [with_client ~host ~port ~on_handler_error handler] connects to the TWS
    software on ([host], [port]) and runs the [handler] until an exception
    is raised or the returned [Deferred.t] is determined.

    [on_handler_error] determines what happens if the [handler] raises an
    exception.

    The standard port for the TWS software is 7496.  You can connect IB Gateway
    on port 4001 as an low-resource alternative compared to TWS.
*)
val with_client
  :  ?do_logging:bool
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

(** Same as [with_client], but returns an [Error] if the connection was not
    successful or an exception was raised in the handler. *)
val with_client_or_error
  :  ?do_logging:bool
  -> ?client_id:Client_id.t
  -> host:string
  -> port:int
  -> (t -> unit Deferred.t)
  -> unit Or_error.t Deferred.t

(** [is_connected t] checks whether the TWS client [t] is connected. *)
val is_connected : t -> bool

(** [state t] returns the connection of the TWS client [t]. *)
val state : t -> [ `Disconnected | `Connecting | `Connected ]

(** [set_server_log_level level] sets the log entry detail [level] of the TWS
    software when processing API requests. *)
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

(** [server_time t] returns the current time from the TWS server or an [Error]. *)
val server_time : t -> Time.t Or_error.t Deferred.t

(** Same as [server_time], but raises an exception if an [Error] was returned. *)
val server_time_exn : t -> Time.t Deferred.t

(** [server_version t] returns the version of the TWS server upon successful
    connection of the TWS client [t], otherwise [None] is returned. *)
val server_version : t -> int option

(** [connection_time t] returns the time the client [t] was connected to TWS or
    [None] when no connection was established. *)
val connection_time : t -> Time.t option

(** [account_code t] returns the code of the Interactive Brokers account upon
    successful connection of the TWS client [t], otherwise [None] is returned. *)
val account_code : t -> Account_code.t option


(** {1 Market data} *)
(******************************************************************************)

module Market_data : sig
  type t =
    [ `Tick_price  of Tick_price.t
    | `Tick_size   of Tick_size.t
    | `Tick_option of Tick_option.t
    | `Tick_string of Tick_string.t
    ] [@@deriving sexp]
  include Response_intf.Wrapper.S with type t := t
  val pp : Format.formatter -> t -> unit
end

(** [market_data t contract] requests streaming market data from TWS for the
    given [contract].  Streaming market data is written to a [Pipe.t] that is
    returned together with a query id used to cancel the request via
    [cancel_market_data] from below.

    When [snapshot] is set to [True] the market data pipe contains only the
    latest snapshot of the data.

    If futher [tick_types] are supplied the marke data pipe is extended with their
    current values. *)
val market_data
  :  ?snapshot:bool (* default is [False] *)
  -> ?tick_types:Tick_type.t list (* default is the empty list *)
  -> t
  -> contract:[< Security_type.t ] Contract.t
  -> (Market_data.t Tws_result.t Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

(** Same as [market_data], but raises an exception instead of returning an
    [Error] explicitly. *)
val market_data_exn
  :  ?snapshot:bool (* default is [False]  *)
  -> ?tick_types:Tick_type.t list (* default is the empty list *)
  -> t
  -> contract:[< Security_type.t ] Contract.t
  -> (Market_data.t Pipe.Reader.t * Query_id.t) Deferred.t

(** [cancel_market_data t query_id] cancels the market data request that
    corresponds to the given [query_id]. *)
val cancel_market_data : t -> Query_id.t -> unit

(** [option_price t contract volatility price] asks TWS to calculate the option
    price for the given contract based on the given [volatility] and current
    [price] of the option's underlying.  An [Error] is returned whenever
    something goes wrong. *)
val option_price
  :  t
  -> contract:[ `Option ] Contract.t
  -> volatility:float
  -> underlying_price:Price.t
  -> Price.t Or_error.t Deferred.t

(** Same as [option_price], but raises an exception in case of an [Error]. *)
val option_price_exn
  :  t
  -> contract:[ `Option ] Contract.t
  -> volatility:float
  -> underlying_price:Price.t
  -> Price.t Deferred.t

(** [implied_volatility t contract option_price under_price] asks TWS to
    calculate the implied volatility for the given option contract based on the
    price of the option and the underlying.  An [Error] is returned whenever
    something goes wrong. *)
val implied_volatility
  :  t
  -> contract:[ `Option ] Contract.t
  -> option_price:Price.t
  -> underlying_price:Price.t
  -> float Or_error.t Deferred.t

(** Same as [implied_volatility], but raises an exception in case of an
    [Error]. *)
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
  -> contract:[< Security_type.t ] Contract.t
  -> order:([< Order_action.t ], [< Order_type.t ]) Order.t
  -> (Order_status.t Tws_result.t Pipe.Reader.t * Order_id.t) Or_error.t Deferred.t

val submit_order_exn
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> order:([< Order_action.t ], [< Order_type.t ]) Order.t
  -> (Order_status.t Pipe.Reader.t * Order_id.t) Deferred.t

val cancel_order_status : t -> Order_id.t -> unit


(** {1 Account and portfolio} *)
(******************************************************************************)

val account_updates : t -> Account_update.t Pipe.Reader.t Or_error.t Deferred.t

val account_updates_exn : t -> Account_update.t Pipe.Reader.t Deferred.t

val portfolio : t -> Position.t Pipe.Reader.t Or_error.t Deferred.t

val portfolio_exn : t -> Position.t Pipe.Reader.t Deferred.t

val commissions : t -> Commission.t Pipe.Reader.t


(** {1 Execution reports} *)
(******************************************************************************)

val executions : t -> Execution.t Pipe.Reader.t

val filter_executions
  :  ?time:Time.t
  -> t
  -> contract:[< Security_type.t ] Contract.t
  -> action:Order_action.t
  -> Execution.t Tws_result.t Pipe.Reader.t Or_error.t Deferred.t

val filter_executions_exn
  :  ?time:Time.t
  -> t
  -> contract:[< Security_type.t ] Contract.t
  -> action:Order_action.t
  -> Execution.t Pipe.Reader.t Deferred.t


(** {1 Contract details} *)
(******************************************************************************)

(** The request returns a pipe containing additional information for all
    contracts that met the criteria specified by the given parameters. *)
val contract_details
  :  t
  -> ?con_id:Contract_id.t
  -> ?multiplier:int
  -> ?prim_exch:Exchange.t
  -> ?local_symbol:Symbol.t
  -> ?sec_id:Security_id.t
  -> ?include_expired:bool
  -> ?exchange:Exchange.t
  -> ?right:Option_right.t
  -> ?expiry:Date.t
  -> ?strike:Price.t
  -> currency:Currency.t
  -> sec_type:[< Security_type.t ]
  -> Symbol.t
  -> Contract_data.t Tws_result.t Pipe.Reader.t Or_error.t Deferred.t

(** Same as [contract_details], but raises an exception instead of returning an
    [Error]. *)
val contract_details_exn
  :  t
  -> ?con_id:Contract_id.t
  -> ?multiplier:int
  -> ?prim_exch:Exchange.t
  -> ?local_symbol:Symbol.t
  -> ?sec_id:Security_id.t
  -> ?include_expired:bool
  -> ?exchange:Exchange.t
  -> ?right:Option_right.t
  -> ?expiry:Date.t
  -> ?strike:Price.t
  -> currency:Currency.t
  -> sec_type:[< Security_type.t ]
  -> Symbol.t
  -> Contract_data.t Pipe.Reader.t Deferred.t

(** [contract_data t contract] returns additional information for the given the
    contract or an [Error]. *)
val contract_data
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> Contract_data.t Or_error.t Deferred.t

(** Same as [contract_data], but raises an exception in case of an [Error]. *)
val contract_data_exn
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> Contract_data.t Deferred.t


(** {1 Futures and option chains} *)
(******************************************************************************)

(** Returns a futures chain for the given contract specification or an [Error].
    It is a special version of [contract_details] from above. *)
val futures_chain
  :  t
  -> ?con_id:Contract_id.t
  -> ?multiplier:int
  -> ?prim_exch:Exchange.t
  -> ?local_symbol:Symbol.t
  -> ?sec_id:Security_id.t
  -> ?include_expired:bool
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> Symbol.t
  -> [> `Futures ] Contract.t list Or_error.t Deferred.t

(** Same as [futures_chain], but raises an exception in case of an [Error]. *)
val futures_chain_exn
  :  t
  -> ?con_id:Contract_id.t
  -> ?multiplier:int
  -> ?prim_exch:Exchange.t
  -> ?local_symbol:Symbol.t
  -> ?sec_id:Security_id.t
  -> ?include_expired:bool
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> Symbol.t
  -> [> `Futures ] Contract.t list Deferred.t

(** Returns an option chain for the given contract specification or an [Error].
    It is a special version of [contract_details] from above. *)
val option_chain
  :  t
  -> ?con_id:Contract_id.t
  -> ?multiplier:int
  -> ?prim_exch:Exchange.t
  -> ?local_symbol:Symbol.t
  -> ?sec_id:Security_id.t
  -> ?include_expired:bool
  -> ?exchange:Exchange.t
  -> ?expiry:Date.t
  -> ?strike:Price.t
  -> ?sec_type:[ `Option | `Fut_opt ] (* Defaults to [Option]. *)
  -> right:Option_right.t
  -> currency:Currency.t
  -> Symbol.t
  -> [> `Option | `Fut_opt ] Contract.t list Or_error.t Deferred.t

(** Same as [option_chain], but raises an exception in case of an [Error]. *)
val option_chain_exn
  :  t
  -> ?con_id:Contract_id.t
  -> ?multiplier:int
  -> ?prim_exch:Exchange.t
  -> ?local_symbol:Symbol.t
  -> ?sec_id:Security_id.t
  -> ?include_expired:bool
  -> ?exchange:Exchange.t
  -> ?expiry:Date.t
  -> ?strike:Price.t
  -> ?sec_type:[ `Option | `Fut_opt ] (* Defaults to [Option]. *)
  -> right:Option_right.t
  -> currency:Currency.t
  -> Symbol.t
  -> [> `Option | `Fut_opt ] Contract.t list Deferred.t


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


(** {1 History} *)
(******************************************************************************)

val history
  :  ?bar_size:[
    | `One_sec     | `Five_sec   | `Fifteen_sec | `Thirty_sec
    | `One_min     | `Two_min    | `Three_min   | `Five_min
    | `Fifteen_min | `Thirty_min | `One_hour    | `One_day
  ] (* defaults to One_day *)
  -> ?duration:[
    | `Sec   of int
    | `Day   of int
    | `Week  of int
    | `Month of int
    | `Year  of int
  ] (* defaults to 1 Year *)
  -> ?use_rth:bool
  -> ?tick_type:[
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
  -> History.t Tws_result.t Or_error.t Deferred.t

val history_exn
  :  ?bar_size:[
    | `One_sec     | `Five_sec   | `Fifteen_sec | `Thirty_sec
    | `One_min     | `Two_min    | `Three_min   | `Five_min
    | `Fifteen_min | `Thirty_min | `One_hour    | `One_day
  ] (* defaults to One_day *)
  -> ?duration:[
    | `Sec   of int
    | `Day   of int
    | `Week  of int
    | `Month of int
    | `Year  of int
  ] (* defaults to 1 Year *)
  -> ?use_rth:bool
  -> ?tick_type:[
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
  -> History.t Deferred.t


(** {1 Realtime bars} *)
(******************************************************************************)

val realtime_bars
  :  ?bar_size:[
    | `Five_sec | `Fifteen_sec | `Thirty_sec
    | `One_min  | `Two_min     | `Three_min
    | `Five_min | `Fifteen_min | `Thirty_min
  ]
  -> ?tick_type:[ `Trades | `Midpoint | `Bid | `Ask ]
  -> ?use_rth:bool
  -> t
  -> contract:[< Security_type.t ] Contract.t
  -> (Bar.t Tws_result.t Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

val realtime_bars_exn
  :  ?bar_size:[
    | `Five_sec | `Fifteen_sec | `Thirty_sec
    | `One_min  | `Two_min     | `Three_min
    | `Five_min | `Fifteen_min | `Thirty_min
  ]
  -> ?tick_type:[ `Trades | `Midpoint | `Bid | `Ask ]
  -> ?use_rth:bool
  -> t
  -> contract:[< Security_type.t ] Contract.t
  -> (Bar.t Pipe.Reader.t * Query_id.t) Deferred.t

val cancel_realtime_bars : t -> Query_id.t -> unit


(** {1 TAQ data} *)
(******************************************************************************)

module Trade : sig
  type t = private
    { stamp : Time.t;
      price : Price.t;
      size  : Volume.t;
    } [@@deriving sexp, fields]

  val pp : Format.formatter -> t -> unit
end

val trades
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> (Trade.t Tws_result.t Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

val trades_exn
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> (Trade.t Pipe.Reader.t * Query_id.t) Deferred.t

val cancel_trades : t -> Query_id.t -> unit

module Quote : sig
  module Change : sig
    type t =
      | Unknown
      | Ask_price of Price.t
      | Bid_price of Price.t
      | Ask_size of Volume.t
      | Bid_size of Volume.t
      | Ask_price_and_size of Price.t * Volume.t
      | Bid_price_and_size of Price.t * Volume.t
    [@@deriving sexp]
  end

  type t = private
    { stamp : Time.t;
      ask_price : Price.t;
      bid_price : Price.t;
      ask_size : Volume.t;
      bid_size : Volume.t;
      change : Change.t;
    } [@@deriving sexp, fields]

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
  [@@deriving sexp]

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

(** [latest_quote t contract] returns either the latest quote for the given
    [contract] or an [Error]. *)
val latest_quote
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> Quote.t Or_error.t Deferred.t

(** Same as [latest_quote] but raises an exception in case of an [Error]. *)
val latest_quote_exn
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> Quote.t Deferred.t

(** [latest_trade t contract] returns either the latest trade for the given
    [contract] or an [Error]. *)
val latest_trade
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> Trade.t Or_error.t Deferred.t

(** Same as [latest_trade] but raises an exception in case of an [Error]. *)
val latest_trade_exn
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> Trade.t Deferred.t


(** {1 Close snapshot} *)
(******************************************************************************)

module Close : sig
  type t = private
    { stamp : Time.t;
      price : Price.t;
    } [@@deriving sexp, fields]
end

(** [latest_close t contract] returns either the latest closing price for the
    given [contract] or an [Error]. *)
val latest_close
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> Close.t Or_error.t Deferred.t

(** Same as [latest_close] but raises an exception in case of an [Error]. *)
val latest_close_exn
  :  t
  -> contract:[< Security_type.t ] Contract.t
  -> Close.t Deferred.t
