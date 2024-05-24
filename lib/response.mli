(** TWS responses *)

open Core

(** {1 Connection and server} *)
(*****************************************************************************)

module Tws_error : sig
  type t = private
    { error_code : int
    ; error_msg : string
    } [@@deriving sexp, fields]
  include Response_intf.S with type t := t

  val create : error_code:int -> error_msg:string -> t
  val to_string_hum : t -> string

  val to_error : t -> Error.t
  val to_exn : t -> exn
  val raise : t -> 'a
end

module Server_time : sig
  type t = Time_float_unix.t [@@deriving sexp]
  include Response_intf.S with type t := t

  val create : time:Time_float_unix.t -> t
end

(** {1 Market data} *)
(*****************************************************************************)

module Tick_price : sig
  module Type : sig
    type t =
      | Bid
      | Ask
      | Last
      | High
      | Low
      | Close
      | Open
    [@@deriving sexp]
  end

  type t = private
    { tick_type : Type.t
    ; price : Price.t
    ; size : Volume.t
    ; can_auto_execute : bool option
    } [@@deriving sexp, fields]
  include Response_intf.S with type t := t

  val create
    :  tick_type:Type.t
    -> price:Price.t
    -> size:Volume.t
    -> can_auto_execute:bool option
    -> t

  val price : t -> Price.t

  val pp : Format.formatter -> t -> unit
end

module Tick_size : sig
  module Type : sig
    type t =
      | Bid
      | Ask
      | Last
      | Volume
    [@@deriving sexp]
  end

  type t = private
    { tick_type : Type.t
    ; size : Volume.t
    } [@@deriving sexp, fields]
  include Response_intf.S with type t := t

  val create : tick_type:Type.t -> size:Volume.t -> t
  val pp : Format.formatter -> t -> unit
end

module Tick_option : sig
  module Type : sig
    type t =
      | Bid
      | Ask
      | Last
      | Model
      | Custom
    [@@deriving sexp]
  end

  type t = private
    { tick_type : Type.t
    ; implied_vol : float
    ; delta : float
    ; option_price : Price.t
    ; pv_dividend : float
    ; gamma : float
    ; vega : float
    ; theta : float
    ; under_price : Price.t
    } [@@deriving sexp, fields]
  include Response_intf.S with type t := t

  val create
    :  tick_type:Type.t
    -> implied_vol:float
    -> delta:float
    -> option_price:Price.t
    -> pv_dividend:float
    -> gamma:float
    -> vega:float
    -> theta:float
    -> under_price:Price.t
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
    [@@deriving sexp]
  end

  type t = private
    { tick_type : Type.t
    ; value : string
    } [@@deriving sexp, fields]
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
      ] [@@deriving sexp]
  end

  type t = private
    { state : State.t
    ; filled : Volume.t
    ; remaining : Volume.t
    ; avg_fill_price : Price.t
    ; permanent_id : int
    ; parent_id : Order_id.t
    ; last_fill_price : Price.t
    ; client_id : Client_id.t
    ; why_held : string option
    } [@@deriving sexp, fields]
  include Response_intf.S with type t := t

  val create
    :  state:State.t
    -> filled:Volume.t
    -> remaining:Volume.t
    -> avg_fill_price:Price.t
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
    { key : string
    ; value : string
    ; currency : string option
    ; account_code : Account_code.t
    } [@@deriving sexp, fields]
  include Response_intf.S with type t := t

  val create
    :  key:string
    -> value:string
    -> currency:string option
    -> account_code:Account_code.t
    -> t
end

module Position : sig
  type t = private
    { contract : Raw_contract.t
    ; size : Volume.t
    ; market_price : Price.t
    ; market_value : Price.t
    ; average_cost : Price.t
    ; unrealized_pnl : Price.t
    ; realized_pnl : Price.t
    ; account_code : Account_code.t
    } [@@deriving sexp, fields]
  include Response_intf.S with type t := t

  val create
    :  contract:[< Security_type.t ] Contract.t
    -> size:Volume.t
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
    { contract : Raw_contract.t
    ; market_name : string
    ; trading_class : string
    ; min_tick : float
    ; order_types : string list
    ; valid_exchanges : Exchange.t list
    ; price_magnifier : int
    ; underlying_id : int
    ; long_name : string
    ; contract_month : string
    ; industry : string
    ; category : string
    ; subcategory : string
    ; time_zone : Time_float_unix.Zone.t option
    ; trading_hours : Trading_times.t list
    ; liquid_hours : Trading_times.t list
    } [@@deriving sexp, fields]
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
    -> time_zone:Time_float_unix.Zone.t
    -> trading_hours:Trading_times.t list
    -> liquid_hours:Trading_times.t list
    -> t

  val contract : t -> [< Security_type.t ] Contract.t

  val regular_trading_times  : t -> Trading_times.t
  val extended_trading_times : t -> Trading_times.t
end

(** {1 Execution} *)
(*****************************************************************************)

module Execution : sig
  module Side : sig
    type t =
      [ `bought
      | `sold
      ] [@@deriving sexp]
    include Stringable.S with type t := t
  end

  type t = private
    { order_id : Order_id.t
    ; contract : Raw_contract.t
    ; exec_id : Execution_id.t
    ; time : Time_float_unix.t
    ; account_code : Account_code.t
    ; exchange : Exchange.t
    ; side : Side.t
    ; volume : Volume.t
    ; price : Price.t
    ; permanent_id : int
    ; client_id : Client_id.t
    ; liquidation : int
    ; cumulative_volume : Volume.t
    ; average_price : Price.t
    ; order_ref : string option
    } [@@deriving sexp, fields]
  include Response_intf.S with type t := t

  val create
    :  order_id:Order_id.t
    -> contract:[< Security_type.t ] Contract.t
    -> exec_id:Execution_id.t
    -> time:Time_float_unix.t
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

  val pp : Format.formatter -> t -> unit
end

module Commission : sig
  type t = private
    { exec_id : Execution_id.t
    ; commission : Price.t
    ; currency : Currency.t
    ; realized_pnl : Price.t
    ; yield : float
    ; yield_redemption_date : int option
    } [@@deriving sexp, fields]
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
    type t =
      | Insert
      | Update
      | Delete
    [@@deriving sexp]
  end

  module Side : sig
    type t =
      | Ask
      | Bid
    [@@deriving sexp]
  end

  type t = private
    { position : int
    ; operation : Operation.t
    ; side : Side.t
    ; price : Price.t
    ; size : Volume.t
    } [@@deriving sexp, fields]
  include Response_intf.S with type t := t

  val create
    :  position:int
    -> operation:Operation.t
    -> side:Side.t
    -> price:Price.t
    -> size:Volume.t
    -> t
end

(** {1 History} *)
(*****************************************************************************)

module History : sig
  type t = private
    { start : Time_float_unix.t
    ; stop : Time_float_unix.t
    ; num_bars : int
    ; bars : Bar.t list
    } [@@deriving sexp, fields]
  include Response_intf.S with type t := t

  val create : bars:Bar.t list -> t

  module Data_frame : sig
    type t = private
      { stamps : Time_float_unix.t array
      ; op : float array
      ; hi : float array
      ; lo : float array
      ; cl : float array
      ; vo : int   array
      } [@@deriving sexp, fields]
  end

  (** [unpack_bars t] unpacks bars into a data frame that stores prices and
      volumes in its columns. *)
  val unpack_bars : t -> Data_frame.t

  (** [time_ohlc t] returns a time series of OHLC bars. *)
  val time_ohlc : t -> (Time_float_unix.t * (float * float * float * float)) list

  (** [time_vwap t] returns a VWAP time series. *)
  val time_vwap : t -> (Time_float_unix.t * float) list
end

(** {1 Realtime bars} *)
(*****************************************************************************)

module Realtime_bar : sig
  type t = Bar.t [@@deriving sexp]
  include Response_intf.S with type t := t
  val create : bar:Bar.t -> t
end
