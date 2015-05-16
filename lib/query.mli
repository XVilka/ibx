(* File: query.mli

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

(** TWS queries *)

open Core.Std
open Std_internal

(** {1 Connection and server} *)
(*****************************************************************************)

module Server_log_level : sig
  module Level : sig
    type t = [ `System | `Error | `Warning | `Information | `Detail ] with sexp
  end
  type t with sexp
  include Query_intf.S with type t := t
  val create : level:Level.t -> t
end

module Server_time : sig
  type t with sexp
  include Query_intf.S with type t := t
  val create : unit -> t
end

(** {1 Market data} *)
(*****************************************************************************)

module Market_data : sig
  module Tick_kind : sig
    type t =
    [ `Option_volume
    | `Option_open_interest
    | `Historical_volatility
    | `Implied_volatility
    | `Index_future_premium
    | `Misc_stats
    | `Mark_price             (* used in TWS P&L computations *)
    | `Auction_values         (* volume, price and imbalance *)
    | `Realtime_volume
    | `Shortable
    | `Inventory
    | `Fundamental_ratios
    | `Turn_off_market_data
    ] with sexp
  end
  type t with sexp
  include Query_intf.S with type t := t
  val create
    :  contract:[< Security_type.t ] Contract.t
    -> tick_generics:Tick_kind.t list
    -> snapshot:bool
    -> t
end

module Calc_option_price : sig
  type t with sexp
  include Query_intf.S with type t := t
  val create
    :  contract:[ `Option ] Contract.t
    -> volatility:float
    -> underlying_price:Price.t
    -> t
end

module Calc_implied_volatility : sig
  type t with sexp
  include Query_intf.S with type t := t
  val create
    :  contract:[ `Option ] Contract.t
    -> option_price:Price.t
    -> underlying_price:Price.t
    -> t
end

(** {1 Orders} *)
(*****************************************************************************)

module Submit_order : sig
  type t with sexp
  include Query_intf.S with type t := t
  val create
    :  contract:[< Security_type.t ] Contract.t
    -> order:([< Order.Action.t ], [< Order.Type.t ]) Order.t
    -> account_code:Account_code.t
    -> t
end

(** {1 Account and portfolio} *)
(*****************************************************************************)

module Account_updates : sig
  type t with sexp
  include Query_intf.S with type t := t
  val create
    :  subscribe:bool
    -> account_code:Account_code.t
    -> t
end

module Positions : sig
  type t with sexp
  include Query_intf.S with type t := t
  val create
    :  subscribe:bool
    -> account_code:Account_code.t
    -> t
end

(** {1 Executions} *)
(*****************************************************************************)

module Executions : sig
  type t with sexp
  include Query_intf.S with type t := t

  val create
    :  contract:[< Security_type.t] Contract.t
    -> client_id:Client_id.t
    -> account_code:Account_code.t
    -> time:Time.t
    -> order_action:Order.Action.t
    -> t
end

(** {1 Contract details} *)
(*****************************************************************************)

module Contract_details : sig
  type t with sexp
  include Query_intf.S with type t := t

  val create
    :  ?contract_id:Contract_id.t
    -> ?multiplier:int
    -> ?listing_exchange:Exchange.t
    -> ?local_symbol:Symbol.t
    -> ?sec_id:Security_id.t
    -> ?include_expired:bool
    -> ?exchange:Exchange.t
    -> ?option_right:Option_right.t
    -> ?expiry:Date.t
    -> ?strike:Price.t
    -> sec_type:Security_type.t
    -> currency:Currency.t
    -> Symbol.t
    -> t
end

(** {1 Market depth} *)
(*****************************************************************************)

module Market_depth : sig
  type t with sexp
  include Query_intf.S with type t := t
  val create
    :  contract:[< Security_type.t ] Contract.t
    -> num_rows:int
    -> t
end

(** {1 Historical data} *)
(*****************************************************************************)

module Historical_data : sig
  module Bar_size : sig
    type t =
    [ `One_sec | `Five_secs | `Fifteen_secs | `Thirty_secs
    | `One_min | `Two_mins | `Three_mins | `Five_mins
    | `Fifteen_mins | `Thirty_mins
    | `One_hour
    | `One_day
    ] with sexp
    include Stringable.S with type t := t
  end

  module Bar_span : sig
    type t =
    [ `Sec of int
    | `Day of int
    | `Week of int
    | `Month of int
    | `Year of int
    ] with sexp
    include Stringable.S with type t := t
  end

  module Show : sig
    type t =
    [ `Trades
    | `Midpoint
    | `Bid
    | `Ask
    | `Bid_ask
    | `Historical_volatility
    | `Implied_volatility
    | `Option_volume
    ] with sexp
    include Stringable.S with type t := t
  end

  type t with sexp
  include Query_intf.S with type t := t
  val create
    :  contract:[< Security_type.t ] Contract.t
    -> until:Time.t
    -> bar_size:Bar_size.t
    -> bar_span:Bar_span.t
    -> use_rth:bool
    -> show:Show.t
    -> t
end

(** {1 Realtime bars} *)
(*****************************************************************************)

module Realtime_bars : sig
  module Bar_size : sig
    type t = [ `Five_secs ] with sexp
  end

  module Show : sig
    type t = [ `Trades | `Midpoint | `Bid | `Ask ]
  end

  type t with sexp
  include Query_intf.S with type t := t
  val create
    :  contract:[< Security_type.t ] Contract.t
    -> bar_size:Bar_size.t
    -> show:Show.t
    -> use_rth:bool
    -> t
end
