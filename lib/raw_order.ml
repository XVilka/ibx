(* File: raw_order.ml

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
open Tws_prot

module Id = struct
  include Unique_id.Int63 (struct end)
  let tws_of_t = to_string
  let t_of_tws = of_string
  let val_type = Val_type.create tws_of_t t_of_tws
  let unpickler =
    Unpickler.create ~name:"Order.Id"
      Unpickler.Spec.(value (required val_type) ~name:"order_id")
      Fn.id
end

module Action = struct
  type t = [ `Buy | `Sell | `Sell_short ] with sexp

  let tws_of_t = function
    | `Buy -> "BUY"
    | `Sell -> "SELL"
    | `Sell_short -> "SSHORT"

  let t_of_tws = function
    | "BUY" -> `Buy
    | "SELL" -> `Sell
    | "SSHORT" -> `Sell_short
    | s -> invalid_argf "Action.t_of_tws: %S" s ()

  let val_type = Val_type.create tws_of_t t_of_tws
end

module Type = struct
  type t = [ `Market | `Limit ] with sexp

  let tws_of_t = function
    | `Market -> "MKT"
    | `Limit -> "LMT"

  let t_of_tws = function
    | "MKT" -> `Market
    | "LMT" -> `Limit
    | s -> invalid_argf "Type.t_of_tws: %S" s ()

  let to_string = tws_of_t
  let of_string = t_of_tws

  let val_type = Val_type.create tws_of_t t_of_tws
end

module Time_in_force = struct
  type t =
  [ `Day
  | `Good_till_cancel
  | `Immediate_or_cancel
  | `Fill_or_kill
  | `Good_till_date_time
  ] with sexp

  let tws_of_t = function
    | `Day -> "DAY"
    | `Good_till_cancel -> "GTC"
    | `Fill_or_kill -> "FOK"
    | `Immediate_or_cancel -> "IOC"
    | `Good_till_date_time -> "GTD"

  let t_of_tws = function
    | "DAY" -> `Day
    | "GTC" -> `Good_till_cancel
    | "FOK" -> `Fill_or_kill
    | "IOC" -> `Immediate_or_cancel
    | "GTD" -> `Good_till_date_time
    | s -> invalid_argf "Time_in_force.t_of_tws: %S" s ()

  let val_type = Val_type.create tws_of_t t_of_tws
end

module Oca_type = struct
  type t =
  [ `cancel_with_block
  | `reduce_with_block
  | `reduce_non_block
  ] with sexp


  let tws_of_t = function
    | `cancel_with_block -> "1"
    | `reduce_with_block -> "2"
    | `reduce_non_block -> "3"

  let t_of_tws = function
    | "1" -> `cancel_with_block
    | "2" -> `reduce_with_block
    | "3" -> `reduce_non_block
    | s -> invalid_argf "Oca_type.t_of_tws: %S" s ()

  let val_type = Val_type.create tws_of_t t_of_tws
end

module Stop_trigger_method = struct
  type t =
  [ `Default
  | `Double_bid_ask
  | `Last
  | `Double_last
  | `Bid_ask
  | `Last_bid_ask
  | `Midpoint
  ] with sexp

  let tws_of_t = function
    | `Default -> "0"
    | `Double_bid_ask -> "1"
    | `Last -> "2"
    | `Double_last -> "3"
    | `Bid_ask -> "4"
    | `Last_bid_ask -> "7"
    | `Midpoint -> "8"

  let t_of_tws = function
    | "0" -> `Default
    | "1" -> `Double_bid_ask
    | "2" -> `Last
    | "3" -> `Double_last
    | "4" -> `Bid_ask
    | "7" -> `Last_bid_ask
    | "8" -> `Midpoint
    | s -> invalid_argf "Stop_trigger_method.t_of_tws: %S" s ()

  let val_type = Val_type.create tws_of_t t_of_tws
end

module Rule80A = struct
  type t =
  [ `Individual
  | `Agency
  | `Agent_other_member
  | `Individual_PTIA
  | `Agency_PTIA
  | `Agent_other_member_PTIA
  | `Individual_PT
  | `Agency_PT
  | `Agent_other_member_PT
  ] with sexp

  let tws_of_t = function
    | `Individual -> "I"
    | `Agency -> "A"
    | `Agent_other_member -> "W"
    | `Individual_PTIA -> "J"
    | `Agency_PTIA -> "U"
    | `Agent_other_member_PTIA -> "M"
    | `Individual_PT -> "K"
    | `Agency_PT -> "Y"
    | `Agent_other_member_PT -> "N"

  let t_of_tws = function
    | "I" -> `Individual
    | "A" -> `Agency
    | "W" -> `Agent_other_member
    | "J" -> `Individual_PTIA
    | "U" -> `Agency_PTIA
    | "M" -> `Agent_other_member_PTIA
    | "K" -> `Individual_PT
    | "Y" -> `Agency_PT
    | "N" -> `Agent_other_member_PT
    | s -> invalid_argf "Rule80A.t_of_tws: %S" s ()

  let val_type = Val_type.create tws_of_t t_of_tws
end

module Open_close = struct
  type t = [ `Open | `Close ] with sexp

  let tws_of_t = function
    | `Open -> "O"
    | `Close -> "C"

  let t_of_tws = function
    | "O" -> `Open
    | "C" -> `Close
    | s -> invalid_argf "Open_close.t_of_tws: %S" s ()

  let val_type = Val_type.create tws_of_t t_of_tws
end

module Origin = struct
  type t = [ `Customer | `Firm ] with sexp

  let tws_of_t = function
    | `Customer -> "0"
    | `Firm -> "1"

  let t_of_tws = function
    | "0" -> `Customer
    | "1" -> `Firm
    | s -> invalid_argf "Origin.t_of_tws: %S" s ()

  let val_type = Val_type.create tws_of_t t_of_tws
end

module Auction_strategy = struct
  type t = [ `Match | `Improvement | `Transparent ] with sexp

  let tws_of_t = function
    | `Match -> "1"
    | `Improvement -> "2"
    | `Transparent -> "3"

  let t_of_tws = function
    | "1" -> `Match
    | "2" -> `Improvement
    | "3" -> `Transparent
    | s -> invalid_argf "Auction_strategy.t_of_tws: %S" s ()

  let val_type = Val_type.create tws_of_t t_of_tws
end

module Volatility_type = struct
  type t = [ `daily | `annual ] with sexp

  let tws_of_t = function
    | `daily -> "1"
    | `annual -> "2"

  let t_of_tws = function
    | "1" -> `daily
    | "2" -> `annual
    | s -> invalid_argf "Volatility_type.t_of_tws: %S" s ()

  let val_type = Val_type.create tws_of_t t_of_tws
end

module Reference_price_type = struct
  type t = [ `average | `bid_or_ask ] with sexp

  let tws_of_t = function
    | `average -> "1"
    | `bid_or_ask -> "2"

  let t_of_tws = function
    | "1" -> `average
    | "2" -> `bid_or_ask
    | s -> invalid_argf "reference_price_type.t_of_tws: %S" s ()

  let val_type = Val_type.create tws_of_t t_of_tws
end

module Hedge_type = struct
  type t = [ `Delta | `Beta | `Fx | `Pair ] with sexp

  let tws_of_t = function
    | `Delta -> "D"
    | `Beta -> "B"
    | `Fx -> "F"
    | `Pair -> "P"

  let t_of_tws = function
    | "D" -> `Delta
    | "B" -> `Beta
    | "F" -> `Fx
    | "P" -> `Pair
    | s -> invalid_argf "Hedge_type.t_of_tws: %S" s ()

  let val_type = Val_type.create tws_of_t t_of_tws
end

module Clearing_intent = struct
  type t = [ `Default | `IB | `Away | `Post_trade_allocation ] with sexp

  let tws_of_t = function
    | `Default -> ""
    | `IB -> "IB"
    | `Away -> "Away"
    | `Post_trade_allocation -> "PTA"

  let t_of_tws = function
    | "" -> `Default
    | "IB" -> `IB
    | "Away" -> `Away
    | "PTA" -> `Post_trade_allocation
    | s -> invalid_argf "Clearing_intent.t_of_tws: %S" s ()

  let val_type = Val_type.create tws_of_t t_of_tws
end

type t =
  { (* ===================== main order fields ==================== *)
    order_id : Id.t;
    action : string;
    quantity : int;
    order_type : string;
    limit_price : Price.t option;
    stop_price : Price.t option;
    (* ================== extended order fields =================== *)
    time_in_force : Time_in_force.t option;
    oca_group_name : string option;
    oca_type : Oca_type.t option;
    order_ref : string option;
    transmit : bool;
    parent_id : Id.t option;
    block_order : bool;
    sweep_to_fill : bool;
    display_size : int option;
    stop_trigger_method : Stop_trigger_method.t;
    outside_regular_trading_hours : bool;
    hidden : bool;
    good_after_date_time : Time.t option;  (* Format: YYYYMMDD hh:mm:ss {time zone} *)
    good_till_date_time : Time.t option;   (* Format: YYYYMMDD hh:mm:ss {time zone} *)
    override_percentage_constraints : bool;
    rule80A : Rule80A.t option;
    all_or_none : bool;
    minimum_quantity : int option;
    percent_offset : float option;
    trailing_stop_price : Price.t option;
    trailing_percent : float option;
    (* ================= financial advisors only ================== *)
    financial_advisor_group : string option;
    financial_advisor_profile : string option;
    financial_advisor_method : string option;
    financial_advisor_percentage : string option;
    (* ================ institutional orders only ================= *)
    open_close : Open_close.t;
    origin : Origin.t;
    short_sale_slot : int option;
    designated_location : string option;
    exemption_code : int;
    (* ================ smart order routing only ================== *)
    discretionary_amount : float option;
    electronic_trade_only : bool;
    firm_quote_only : bool;
    nbbo_price_cap : float option;
    opt_out_smart_routing : bool;
    (* ================= box or vol orders only =================== *)
    auction_strategy : Auction_strategy.t option;
    (* ==================== box orders only ======================= *)
    starting_price : Price.t option;
    stock_reference_price : Price.t option;
    delta : float option;
    (* ============= pegged to stock or VOL orders ================ *)
    lower_stock_price_range : Price.t option;
    upper_stock_price_range : Price.t option;
    (* ================= volatility orders only =================== *)
    volatility : float option;
    volatility_type : Volatility_type.t option;
    continuous_update : bool;
    reference_price_type : Reference_price_type.t option;
    delta_neutral_order_type : Type.t option;
    delta_neutral_aux_price : Price.t option;
    delta_neutral_contract_id : Contract.Id.t option;
    delta_neutral_settling_firm : string option;
    delta_neutral_clearing_account : string option;
    delta_neutral_clearing_intent : string option;
    (* ================ combination orders only =================== *)
    basis_points : float option;
    basis_points_type : int option;
    (* ==================== scale orders only ===================== *)
    scale_initial_level_size : int option;
    scale_subsequent_level_size : int option;
    scale_price_increment : float option;
    scale_price_adjust_value : float option;
    scale_price_adjust_interval : int option;
    scale_profit_offset : float option;
    scale_auto_reset : bool;
    scale_init_position : int option;
    scale_init_fill_quantity : int option;
    scale_random_percent : bool;
    (* ==================== hedge orders only ===================== *)
    hedge_type : Hedge_type.t option;
    hedge_parameter : string option;
    (* ====================== clearing info ======================= *)
    account_code : Account_code.t option; (* IB account name *)
    settling_firm : string option;
    clearing_account : string option; (* True beneficiary of the order *)
    clearing_intent : Clearing_intent.t option;
    (* ==================== algo orders only ====================== *)
    algo_strategy : string option;
    request_pre_trade_information : bool;
    not_held : bool;
  }
with sexp, fields

let create
    ?limit_price
    ?stop_price
    ?time_in_force
    ?oca_group_name
    ?oca_type
    ?order_ref
    ?(transmit=true)
    ?parent_id
    ?(block_order=false)
    ?(sweep_to_fill=false)
    ?display_size
    ?(stop_trigger_method=`Default)
    ?(outside_regular_trading_hours=false)
    ?(hidden=false)
    ?good_after_date_time
    ?good_till_date_time
    ?(override_percentage_constraints=false)
    ?rule80A
    ?(all_or_none=false)
    ?minimum_quantity
    ?percent_offset
    ?trailing_stop_price
    ?trailing_percent
    ?financial_advisor_group
    ?financial_advisor_profile
    ?financial_advisor_method
    ?financial_advisor_percentage
    ?(open_close=`Open)
    ?(origin=`Customer)
    ?short_sale_slot
    ?designated_location
    ?(exemption_code=(-1))
    ?discretionary_amount
    ?(electronic_trade_only=false)
    ?(firm_quote_only=false)
    ?nbbo_price_cap
    ?(opt_out_smart_routing=false)
    ?auction_strategy ?starting_price
    ?stock_reference_price
    ?delta
    ?lower_stock_price_range
    ?upper_stock_price_range
    ?volatility
    ?volatility_type
    ?(continuous_update=false)
    ?reference_price_type
    ?delta_neutral_order_type
    ?delta_neutral_aux_price
    ?delta_neutral_contract_id
    ?delta_neutral_settling_firm
    ?delta_neutral_clearing_account
    ?delta_neutral_clearing_intent
    ?basis_points ?basis_points_type
    ?scale_initial_level_size
    ?scale_subsequent_level_size
    ?scale_price_increment
    ?scale_price_adjust_value
    ?scale_price_adjust_interval
    ?scale_profit_offset
    ?(scale_auto_reset=false)
    ?scale_init_position
    ?scale_init_fill_quantity
    ?(scale_random_percent=false)
    ?hedge_type
    ?hedge_parameter
    ?account_code
    ?settling_firm
    ?clearing_account
    ?clearing_intent
    ?algo_strategy
    ?(request_pre_trade_information=false)
    ?(not_held=false)
    ~action
    ~order_type
    ~quantity
    () =
  { order_id = Id.create ();
    action;
    quantity;
    order_type;
    limit_price;
    stop_price;
    time_in_force;
    oca_group_name;
    oca_type;
    order_ref;
    transmit;
    parent_id;
    block_order;
    sweep_to_fill;
    display_size;
    stop_trigger_method;
    outside_regular_trading_hours;
    hidden;
    good_after_date_time;
    good_till_date_time;
    override_percentage_constraints;
    rule80A;
    all_or_none;
    minimum_quantity;
    percent_offset;
    trailing_stop_price;
    trailing_percent;
    financial_advisor_group;
    financial_advisor_profile;
    financial_advisor_method;
    financial_advisor_percentage;
    open_close;
    origin;
    short_sale_slot;
    designated_location;
    exemption_code;
    discretionary_amount;
    electronic_trade_only;
    firm_quote_only;
    nbbo_price_cap;
    opt_out_smart_routing;
    auction_strategy;
    starting_price;
    stock_reference_price;
    delta;
    lower_stock_price_range;
    upper_stock_price_range;
    volatility;
    volatility_type;
    continuous_update;
    reference_price_type;
    delta_neutral_order_type;
    delta_neutral_aux_price;
    delta_neutral_contract_id;
    delta_neutral_settling_firm;
    delta_neutral_clearing_account;
    delta_neutral_clearing_intent;
    basis_points;
    basis_points_type;
    scale_initial_level_size;
    scale_subsequent_level_size;
    scale_price_increment;
    scale_price_adjust_value;
    scale_price_adjust_interval;
    scale_profit_offset;
    scale_auto_reset;
    scale_init_position;
    scale_init_fill_quantity;
    scale_random_percent;
    hedge_type;
    hedge_parameter;
    account_code;
    settling_firm;
    clearing_account;
    clearing_intent;
    algo_strategy;
    request_pre_trade_information;
    not_held;
  }

let ( = ) t1 t2 : bool =
  let use op = fun field ->
    op (Field.get field t1) (Field.get field t2)
  in
  Fields.for_all
    ~order_id:(use Id.(=))
    ~action:(use (=))
    ~quantity:(use (=))
    ~order_type:(use (=))
    ~limit_price:(use (Option.equal Price.(=.)))
    ~stop_price:(use (Option.equal Price.(=.)))
    ~time_in_force:(use (=))
    ~oca_group_name:(use (=))
    ~oca_type:(use (=))
    ~order_ref:(use (=))
    ~transmit:(use (=))
    ~parent_id:(use (Option.equal Id.(=)))
    ~block_order:(use (=))
    ~sweep_to_fill:(use (=))
    ~display_size:(use (=))
    ~stop_trigger_method:(use (=))
    ~outside_regular_trading_hours:(use (=))
    ~hidden:(use (=))
    ~good_after_date_time:(use (Option.equal Time.(=)))
    ~good_till_date_time:(use (Option.equal Time.(=)))
    ~override_percentage_constraints:(use (=))
    ~rule80A:(use (=))
    ~all_or_none:(use (=))
    ~minimum_quantity:(use (=))
    ~percent_offset:(use (Option.equal Float.(=.)))
    ~trailing_stop_price:(use (Option.equal Price.(=.)))
    ~trailing_percent:(use (Option.equal Float.(=.)))
    ~financial_advisor_group:(use (=))
    ~financial_advisor_method:(use (=))
    ~financial_advisor_percentage:(use (=))
    ~financial_advisor_profile:(use (=))
    ~open_close:(use (=))
    ~origin:(use (=))
    ~short_sale_slot:(use (=))
    ~designated_location:(use (=))
    ~exemption_code:(use (=))
    ~discretionary_amount:(use (Option.equal Float.(=.)))
    ~electronic_trade_only:(use (=))
    ~firm_quote_only:(use (=))
    ~nbbo_price_cap:(use (Option.equal Float.(=.)))
    ~opt_out_smart_routing:(use (=))
    ~auction_strategy:(use (=))
    ~starting_price:(use (Option.equal Price.(=.)))
    ~stock_reference_price:(use (Option.equal Price.(=.)))
    ~delta:(use (Option.equal Float.(=.)))
    ~lower_stock_price_range:(use (Option.equal Price.(=.)))
    ~upper_stock_price_range:(use (Option.equal Price.(=.)))
    ~volatility:(use (Option.equal Float.(=.)))
    ~volatility_type:(use (=))
    ~continuous_update:(use (=))
    ~reference_price_type:(use (=))
    ~delta_neutral_order_type:(use (=))
    ~delta_neutral_aux_price:(use (Option.equal Price.(=.)))
    ~delta_neutral_contract_id:(use (Option.equal Contract.Id.(=)))
    ~delta_neutral_settling_firm:(use (=))
    ~delta_neutral_clearing_account:(use (=))
    ~delta_neutral_clearing_intent:(use (=))
    ~basis_points:(use (=))
    ~basis_points_type:(use (=))
    ~scale_initial_level_size:(use (=))
    ~scale_subsequent_level_size:(use (=))
    ~scale_price_increment:(use (Option.equal Float.(=.)))
    ~scale_price_adjust_value:(use (Option.equal Float.(=.)))
    ~scale_price_adjust_interval:(use (=))
    ~scale_profit_offset:(use (Option.equal Float.(=.)))
    ~scale_auto_reset:(use (=))
    ~scale_init_position:(use (=))
    ~scale_init_fill_quantity:(use (=))
    ~scale_random_percent:(use (=))
    ~hedge_type:(use (=))
    ~hedge_parameter:(use (=))
    ~account_code:(use (Option.equal Account_code.(=)))
    ~settling_firm:(use (=))
    ~clearing_account:(use (=))
    ~clearing_intent:(use (=))
    ~algo_strategy:(use (=))
    ~request_pre_trade_information:(use (=))
    ~not_held:(use (=))
