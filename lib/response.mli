(* File: response.mli

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

(** TWS responses *)

open Core.Std
open Std_internal

(** {1 Connection and server} *)
(*****************************************************************************)

module Tws_error : sig
  type t = private
    { error_code : int;
      error_msg : string;
    }
  with sexp, fields
  include Response_intf.S with type t := t

  val create : error_code:int -> error_msg:string -> t
  val to_string_hum : t -> string

  val to_error : t -> Error.t
  val to_exn : t -> exn
  val raise : t -> 'a
end

module Server_time : sig
  type t = Time.t with sexp
  include Response_intf.S with type t := t

  val create : time:Time.t -> t
end

(** {1 Market data} *)
(*****************************************************************************)

module Tick_price : sig
  module Type : sig
    type t = Bid | Ask | Last | High | Low | Close | Open with sexp
  end
  type t = private
    { tick_type : Type.t;
      price : Price.t;
      size : Volume.t;
      can_auto_execute : bool option;
    }
  with sexp, fields
  include Response_intf.S with type t := t

  val create
    :  tick_type:Type.t
    -> price:Price.t
    -> size:Volume.t
    -> can_auto_execute:bool option
    -> t

  val pp : Format.formatter -> t -> unit
end

module Tick_size : sig
  module Type : sig
    type t = Bid | Ask | Last | Volume with sexp
  end
  type t = private
    { tick_type : Type.t;
      size : Volume.t;
    }
  with sexp, fields
  include Response_intf.S with type t := t

  val create : tick_type:Type.t -> size:Volume.t -> t
  val pp : Format.formatter -> t -> unit
end

module Tick_option : sig
  module Type : sig
    type t = Bid | Ask | Last | Model | Custom with sexp
  end
  type t = private
    { tick_type : Type.t;
      implied_volatility : float option;
      delta : float option;
      option_price : Price.t option;
      pv_dividend : float option;
      gamma : float option;
      vega : float option;
      theta : float option;
      underlying_price : Price.t option;
    }
  with sexp, fields
  include Response_intf.S with type t := t

  val create
    :  tick_type:Type.t
    -> implied_volatility:float option
    -> delta:float option
    -> option_price:Price.t option
    -> pv_dividend:float option
    -> gamma:float option
    -> vega:float option
    -> theta:float option
    -> underlying_price:Price.t option
    -> t

  val pp : Format.formatter -> t -> unit
end

module Tick_string : sig
  module Type : sig
    type t =
    | Bid_size
    | Bid_price
    | Ask_price
    | Ask_size
    | Last_price
    | Last_size
    | High_price
    | Low_price
    | Volume
    | Close_price
    | Bid_option
    | Ask_option
    | Last_option
    | Model_option
    | Open_price
    | Low_13_week
    | High_13_week
    | Low_26_week
    | High_26_week
    | Low_52_week
    | High_52_week
    | Avg_volume
    | Open_interest
    | Historical_volatility
    | Implied_volatility
    | Option_bid_exch
    | Option_ask_exch
    | Call_open_interest
    | Put_open_interest
    | Call_volume
    | Put_volume
    | Index_future_premium
    | Bid_exch
    | Ask_exch
    | Auction_volume
    | Auction_price
    | Auction_imbalance
    | Mark_price
    | Bid_efp
    | Ask_efp
    | Last_efp
    | Open_efp
    | High_efp
    | Low_efp
    | Close_efp
    | Last_timestamp
    | Shortable
    | Fundamental_ratios
    | Realtime_volume
    | Halted
    | Bid_yield
    | Ask_yield
    | Last_yield
    | Cust_option_comp
    with sexp
  end
  type t = private
    { tick_type : Type.t;
      value : string;
    }
  with sexp, fields
  include Response_intf.S with type t := t

  val create : tick_type:Type.t -> value:string -> t
  val pp : Format.formatter -> t -> unit
end

(** {1 Orders} *)
(*****************************************************************************)

module Order_status : sig
  module State : sig
    type t =
    [ `Pending_submit
    | `Pending_cancel
    | `Pre_submitted
    | `Submitted
    | `Cancelled
    | `Filled
    | `Inactive
    ] with sexp
  end
  type t = private
    { state : State.t;
      filled : int;
      remaining : int;
      average_fill_price : Price.t;
      permanent_id : int;
      parent_id : Order_id.t;
      last_fill_price : Price.t;
      client_id : Client_id.t;
      why_held : string option;
    }
  with sexp, fields
  include Response_intf.S with type t := t

  val create
    :  state:State.t
    -> filled:int
    -> remaining:int
    -> average_fill_price:Price.t
    -> permanent_id:int
    -> parent_id:Order_id.t
    -> last_fill_price:Price.t
    -> client_id:Client_id.t
    -> why_held:string option
    -> t
end

(** {1 Account and Portfolio} *)
(*****************************************************************************)

module Account_update : sig
  type t = private
    { key : string;
      value : string;
      currency : string option;
      account_code : Account_code.t;
    }
  with sexp, fields
  include Response_intf.S with type t := t

  val create
    :  key:string
    -> value:string
    -> currency:string option
    -> account_code:Account_code.t
    -> t
end

module Portfolio_position : sig
  type t = private
    { contract : Raw_contract.t;
      volume : Volume.t;
      market_price : Price.t;
      market_value : Price.t;
      average_cost : Price.t;
      unrealized_pnl : Price.t;
      realized_pnl : Price.t;
      account_code : Account_code.t;
    }
  with sexp, fields
  include Response_intf.S with type t := t

  val create
    :  contract:[< Security_type.t ] Contract.t
    -> volume:Volume.t
    -> market_price:Price.t
    -> market_value:Price.t
    -> average_cost:Price.t
    -> unrealized_pnl:Price.t
    -> realized_pnl:Price.t
    -> account_code:Account_code.t
    -> t

  val contract : t -> [< Security_type.t ] Contract.t

  (* [total_pnl position] computes the total P&L of a portfolio [position], ie
     unrealized P&L + realized P&L. *)
  val total_pnl : t -> Price.t

  (** [return position] calculates the return of a portfolio [position], ie
      sign(position) * (market_value / (average_cost * position) - 1) *)
  val return : t -> float
end

(** {1 Contract details} *)
(*****************************************************************************)

module Contract_data : sig
  type t = private
    { contract : Raw_contract.t;
      market_name : string;
      trading_class : string;
      min_tick : float;
      order_types : string list;
      valid_exchanges : Exchange.t list;
      price_magnifier : int;
      underlying_id : int;
      long_name : string;
      contract_month : string;
      industry : string;
      category : string;
      subcategory : string;
      time_zone : Time.Zone.t;
      trading_hours : string;
      liquid_hours : string;
    } with sexp, fields
  include Response_intf.S with type t := t

  val create
    :  contract:[< Security_type.t ] Contract.t
    -> market_name:string
    -> trading_class:string
    -> min_tick:float
    -> order_types:string list
    -> valid_exchanges:Exchange.t list
    -> price_magnifier:int
    -> underlying_id:int
    -> long_name:string
    -> contract_month:string
    -> industry:string
    -> category:string
    -> subcategory:string
    -> time_zone:Time.Zone.t
    -> trading_hours:string
    -> liquid_hours:string
    -> t

  val contract : t -> [< Security_type.t ] Contract.t
end

(** {1 Execution} *)
(*****************************************************************************)

module Execution : sig
  module Side : sig
    type t = [ `bought | `sold ] with sexp
    include Stringable.S with type t := t
  end

  type t = private
    { order_id : Order_id.t;
      contract : Raw_contract.t;
      exec_id : Execution_id.t;
      time : Time.t;
      account_code : Account_code.t;
      exchange : Exchange.t;
      side : Side.t;
      volume : Volume.t;
      price : Price.t;
      permanent_id : int;
      client_id : Client_id.t;
      liquidation : int;
      cumulative_volume : Volume.t;
      average_price : Price.t;
      order_ref : string option;
    }
  with sexp, fields
  include Response_intf.S with type t := t

  val create
    :  order_id:Order_id.t
    -> contract:[< Security_type.t ] Contract.t
    -> exec_id:Execution_id.t
    -> time:Time.t
    -> account_code:Account_code.t
    -> exchange:Exchange.t
    -> side:Side.t
    -> volume:Volume.t
    -> price:Price.t
    -> permanent_id:int
    -> client_id:Client_id.t
    -> liquidation:int
    -> cumulative_volume:Volume.t
    -> average_price:Price.t
    -> order_ref:string option
    -> t

  val contract : t -> [< Security_type.t ] Contract.t
end

module Commission : sig
  type t = private
    { exec_id : Execution_id.t;
      commission : Price.t;
      currency : Currency.t;
      realized_pnl : Price.t;
      yield : float;
      yield_redemption_date : int option;
    } with sexp, fields
  include Response_intf.S with type t := t

  val create
    :  exec_id:Execution_id.t
    -> commission:Price.t
    -> currency:Currency.t
    -> realized_pnl:Price.t
    -> yield:float
    -> yield_redemption_date:int option
    -> t
end

(** {1 Market depth} *)
(*****************************************************************************)

module Book_update : sig
  module Operation : sig
    type t = Insert | Update | Delete with sexp
  end
  module Side : sig
    type t = Ask | Bid with sexp
  end
  type t = private
    { position : int;
      operation : Operation.t;
      side : Side.t;
      price : Price.t;
      size : Volume.t;
    }
  with sexp, fields
  include Response_intf.S with type t := t

  val create
    :  position:int
    -> operation:Operation.t
    -> side:Side.t
    -> price:Price.t
    -> size:Volume.t
    -> t
end

(** {1 Historical data} *)
(*****************************************************************************)

module Historical_data : sig
  module Bar : sig
    type t =
      { stamp : Time.t;
        op : Price.t;
        hi : Price.t;
        lo : Price.t;
        cl : Price.t;
        volume : Volume.t;
        wap : Price.t;
        has_gaps : bool;
        count : int;
      }
    with sexp, fields
    include Response_intf.S with type t := t

    val create
      :  stamp:Time.t
      -> op:Price.t
      -> hi:Price.t
      -> lo:Price.t
      -> cl:Price.t
      -> volume:Volume.t
      -> wap:Price.t
      -> has_gaps:bool
      -> count:int
      -> t
  end

  type t =
    { start_time : Time.t;
      end_time : Time.t;
      num_bars : int;
      bars : Bar.t list;
    }
  with sexp, fields
  include Response_intf.S with type t := t

  val create
    :  start_time:Time.t
    -> end_time:Time.t
    -> bars:Bar.t list
    -> t

  module Data_frame : sig
    type t with sexp
    val stamps    : t -> Time.t array
    val op_prices : t -> float array
    val hi_prices : t -> float array
    val lo_prices : t -> float array
    val cl_prices : t -> float array
    val volumes   : t -> int array
  end

  val to_data_frame : t -> Data_frame.t
end

(** {1 Realtime bars} *)
(*****************************************************************************)

module Realtime_bar : sig
  type t =
    { stamp : Time.t;
      op : Price.t;
      hi : Price.t;
      lo : Price.t;
      cl : Price.t;
      volume : Volume.t;
      wap : Price.t;
      count : int;
    }
  with sexp, fields
  include Response_intf.S with type t := t

  val create
    :  stamp:Time.t
    -> op:Price.t
    -> hi:Price.t
    -> lo:Price.t
    -> cl:Price.t
    -> volume:Volume.t
    -> wap:Price.t
    -> count:int
    -> t
end
