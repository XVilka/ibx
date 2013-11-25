(* File: sumbmit_order.ml

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

type t =
  { (* ===================== contract fields ==================== *)
    contract_id : Raw_contract.Id.t option;
    symbol : Symbol.t;
    contract_type : string;
    expiry : Date.t option;
    strike : Price.t option;
    option_right : Raw_contract.Option_right.t option;
    multiplier : string option;
    exchange : Exchange.t;
    listing_exchange : Exchange.t option;
    currency : Currency.t;
    local_symbol : Symbol.t option;
    security_id_type : Raw_contract.Security_id.Type.t option;
    security_id : Raw_contract.Security_id.t option;
    (* ====================== order fields ====================== *)
    order_action : string;
    quantity : int;
    order_kind : string;
    limit_price : Price.t option;
    stop_price : Price.t option;
    time_in_force : Raw_order.Time_in_force.t option;
    oca_group_name : string option;
    account_code : Account_code.t;
    open_close : Raw_order.Open_close.t;
    origin : Raw_order.Origin.t;
    order_ref : string option;
    transmit : bool;
    parent_id : Raw_order.Id.t option;
    block_order : bool;
    sweep_to_fill : bool;
    display_size : int option;
    stop_trigger_method : Raw_order.Stop_trigger_method.t;
    outside_regular_trading_hours : bool;
    hidden : bool;
    shares_allocation : string;
    discretionary_amount : float option;
    good_after_date_time : Time.t option;
    good_till_date_time : Time.t option;
    financial_advisor_group : string option;
    financial_advisor_method : string option;
    financial_advisor_percentage : string option;
    financial_advisor_profile : string option;
    short_sale_slot : int option;
    designated_location : string option;
    exemption_code : int;
    oca_type : Raw_order.Oca_type.t option;
    rule80A : Raw_order.Rule80A.t option;
    settling_firm : string option;
    all_or_none : bool;
    minimum_quantity : int option;
    percent_offset : float option;
    electronic_trade_only : bool;
    firm_quote_only : bool;
    nbbo_price_cap : float option;
    auction_strategy : Raw_order.Auction_strategy.t option;
    starting_price : Price.t option;
    stock_reference_price : Price.t option;
    delta : float option;
    lower_stock_price_range : Price.t option;
    upper_stock_price_range : Price.t option;
    override_percentage_constraints : bool;
    volatility : float option;
    volatility_type : Raw_order.Volatility_type.t option;
    delta_neutral_order_type : Raw_order.Type.t option;
    delta_neutral_aux_price : Price.t option;
    continuous_update : bool;
    reference_price_type : Raw_order.Reference_price_type.t option;
    trailing_stop_price : Price.t option;
    trailing_percent : float option;
    scale_initial_level_size : int option;
    scale_subsequent_level_size : int option;
    scale_price_increment : float option;
    hedge_type : Raw_order.Hedge_type.t option;
    opt_out_smart_routing : bool;
    clearing_account : string option;
    clearing_intent : Raw_order.Clearing_intent.t option;
    not_held : bool;
    underlying_combo : bool;
    algo_strategy : string option;
    request_pre_trade_information : bool;
  }
with sexp, fields

let create ~contract ~order ~account_code =
  let contract = Contract.to_raw contract in
  let order = Order.to_raw order in
  { (* ========================== contract fields =========================== *)
    contract_id                     = contract.Raw_contract.contract_id;
    symbol                          = contract.Raw_contract.symbol;
    contract_type                   = contract.Raw_contract.contract_type;
    expiry                          = contract.Raw_contract.expiry;
    strike                          = contract.Raw_contract.strike;
    option_right                    = contract.Raw_contract.option_right;
    multiplier                      = contract.Raw_contract.multiplier;
    exchange                        = contract.Raw_contract.exchange;
    listing_exchange                = contract.Raw_contract.listing_exchange;
    currency                        = contract.Raw_contract.currency;
    local_symbol                    = contract.Raw_contract.local_symbol;
    security_id_type                = contract.Raw_contract.security_id_type;
    security_id                     = contract.Raw_contract.security_id;
    (* =========================== order fields ============================= *)
    order_action                    = order.Raw_order.action;
    quantity                        = order.Raw_order.quantity;
    order_kind                      = order.Raw_order.order_type;
    limit_price                     = order.Raw_order.limit_price;
    stop_price                      = order.Raw_order.stop_price;
    time_in_force                   = order.Raw_order.time_in_force;
    oca_group_name                  = order.Raw_order.oca_group_name;
    account_code;  (* The account code of the current session. *)
    open_close                      = order.Raw_order.open_close;
    origin                          = order.Raw_order.origin;
    order_ref                 = order.Raw_order.order_ref;
    transmit                        = order.Raw_order.transmit;
    parent_id                       = order.Raw_order.parent_id;
    block_order                     = order.Raw_order.block_order;
    sweep_to_fill                   = order.Raw_order.sweep_to_fill;
    display_size                    = order.Raw_order.display_size;
    stop_trigger_method             = order.Raw_order.stop_trigger_method;
    outside_regular_trading_hours   = order.Raw_order.outside_regular_trading_hours;
    hidden                          = order.Raw_order.hidden;
    shares_allocation               = ""; (* This field is deprecated. *)
    discretionary_amount            = order.Raw_order.discretionary_amount;
    good_after_date_time            = order.Raw_order.good_after_date_time;
    good_till_date_time             = order.Raw_order.good_till_date_time;
    financial_advisor_group         = order.Raw_order.financial_advisor_group;
    financial_advisor_method        = order.Raw_order.financial_advisor_method;
    financial_advisor_percentage    = order.Raw_order.financial_advisor_percentage;
    financial_advisor_profile       = order.Raw_order.financial_advisor_profile;
    short_sale_slot                 = order.Raw_order.short_sale_slot;
    designated_location             = order.Raw_order.designated_location;
    exemption_code                  = order.Raw_order.exemption_code;
    oca_type                        = order.Raw_order.oca_type;
    rule80A                         = order.Raw_order.rule80A;
    settling_firm                   = order.Raw_order.settling_firm;
    all_or_none                     = order.Raw_order.all_or_none;
    minimum_quantity                = order.Raw_order.minimum_quantity;
    percent_offset                  = order.Raw_order.percent_offset;
    electronic_trade_only           = order.Raw_order.electronic_trade_only;
    firm_quote_only                 = order.Raw_order.firm_quote_only;
    nbbo_price_cap                  = order.Raw_order.nbbo_price_cap;
    auction_strategy                = order.Raw_order.auction_strategy;
    starting_price                  = order.Raw_order.starting_price;
    stock_reference_price           = order.Raw_order.stock_reference_price;
    delta                           = order.Raw_order.delta;
    lower_stock_price_range         = order.Raw_order.lower_stock_price_range;
    upper_stock_price_range         = order.Raw_order.upper_stock_price_range;
    override_percentage_constraints = order.Raw_order.override_percentage_constraints;
    volatility                      = order.Raw_order.volatility;
    volatility_type                 = order.Raw_order.volatility_type;
    delta_neutral_order_type        = order.Raw_order.delta_neutral_order_type;
    delta_neutral_aux_price         = order.Raw_order.delta_neutral_aux_price;
    continuous_update               = order.Raw_order.continuous_update;
    reference_price_type            = order.Raw_order.reference_price_type;
    trailing_stop_price             = order.Raw_order.trailing_stop_price;
    trailing_percent                = order.Raw_order.trailing_percent;
    scale_initial_level_size        = order.Raw_order.scale_initial_level_size;
    scale_subsequent_level_size     = order.Raw_order.scale_subsequent_level_size;
    scale_price_increment           = order.Raw_order.scale_price_increment;
    hedge_type                      = order.Raw_order.hedge_type;
    opt_out_smart_routing           = order.Raw_order.opt_out_smart_routing;
    clearing_account                = order.Raw_order.clearing_account;
    clearing_intent                 = order.Raw_order.clearing_intent;
    not_held                        = order.Raw_order.not_held;
    underlying_combo                = false;
    algo_strategy                   = order.Raw_order.algo_strategy;
    request_pre_trade_information   = order.Raw_order.request_pre_trade_information;
  }

let ( = ) t1 t2 : bool =
  let use op = fun field ->
    op (Field.get field t1) (Field.get field t2)
  in
  Fields.for_all
    ~contract_id:(use (Option.equal Raw_contract.Id.(=)))
    ~symbol:(use Symbol.(=))
    ~contract_type:(use (=))
    ~expiry:(use (=))
    ~strike:(use (Option.equal Price.(=.)))
    ~option_right:(use (=))
    ~multiplier:(use (=))
    ~exchange:(use (=))
    ~listing_exchange:(use (=))
    ~currency:(use (=))
    ~local_symbol:(use (=))
    ~security_id_type:(use (=))
    ~security_id:(use (=))
    ~order_action:(use (=))
    ~quantity:(use (=))
    ~order_kind:(use (=))
    ~limit_price:(use (Option.equal Price.(=.)))
    ~stop_price:(use (Option.equal Price.(=.)))
    ~time_in_force:(use (=))
    ~oca_group_name:(use (=))
    ~account_code:(use Account_code.(=))
    ~open_close:(use (=))
    ~origin:(use (=))
    ~order_ref:(use (=))
    ~transmit:(use (=))
    ~parent_id:(use (Option.equal Raw_order.Id.(=)))
    ~block_order:(use (=))
    ~sweep_to_fill:(use (=))
    ~display_size:(use (=))
    ~stop_trigger_method:(use (=))
    ~outside_regular_trading_hours:(use (=))
    ~hidden:(use (=))
    ~shares_allocation:(use (=))
    ~discretionary_amount:(use (Option.equal Float.(=.)))
    ~good_after_date_time:(use (Option.equal Time.(=)))
    ~good_till_date_time:(use (Option.equal Time.(=)))
    ~financial_advisor_group:(use (=))
    ~financial_advisor_method:(use (=))
    ~financial_advisor_percentage:(use (=))
    ~financial_advisor_profile:(use (=))
    ~short_sale_slot:(use (=))
    ~designated_location:(use (=))
    ~exemption_code:(use (=))
    ~oca_type:(use (=))
    ~rule80A:(use (=))
    ~settling_firm:(use (=))
    ~all_or_none:(use (=))
    ~minimum_quantity:(use (=))
    ~percent_offset:(use (Option.equal Float.(=.)))
    ~electronic_trade_only:(use (=))
    ~firm_quote_only:(use (=))
    ~nbbo_price_cap:(use (Option.equal Float.(=.)))
    ~auction_strategy:(use (=))
    ~starting_price:(use (Option.equal Price.(=.)))
    ~stock_reference_price:(use (Option.equal Price.(=.)))
    ~delta:(use (Option.equal Float.(=.)))
    ~lower_stock_price_range:(use (Option.equal Price.(=.)))
    ~upper_stock_price_range:(use (Option.equal Price.(=.)))
    ~override_percentage_constraints:(use (=))
    ~volatility:(use (Option.equal Float.(=.)))
    ~volatility_type:(use (=))
    ~delta_neutral_order_type:(use (=))
    ~delta_neutral_aux_price:(use (Option.equal Price.(=.)))
    ~continuous_update:(use (=))
    ~reference_price_type:(use (=))
    ~trailing_stop_price:(use (Option.equal Price.(=.)))
    ~trailing_percent:(use (Option.equal Float.(=.)))
    ~scale_initial_level_size:(use (=))
    ~scale_subsequent_level_size:(use (=))
    ~scale_price_increment:(use (Option.equal Float.(=.)))
    ~hedge_type:(use (=))
    ~opt_out_smart_routing:(use (=))
    ~clearing_account:(use (=))
    ~clearing_intent:(use (=))
    ~not_held:(use (=))
    ~underlying_combo:(use (=))
    ~algo_strategy:(use (=))
    ~request_pre_trade_information:(use (=))

let pickler = lazy (
  Pickler.create ~name:"Submit-Order"
    Pickler.Spec.(
      wrap (
        Fields.fold
          ~init:(empty ())
          ~contract_id:(fields_value (optional Raw_contract.Id.val_type ~default_on_none:"0"))
          ~symbol:(fields_value (required Symbol.val_type))
          ~contract_type:(fields_value (required string))
          ~expiry:(fields_value (optional date))
          ~strike:(fields_value (optional Price.val_type ~default_on_none:"0.0"))
          ~option_right:(fields_value (optional Raw_contract.Option_right.val_type))
          ~multiplier:(fields_value (optional string))
          ~exchange:(fields_value (required Exchange.val_type))
          ~listing_exchange:(fields_value (optional Exchange.val_type))
          ~currency:(fields_value (required Currency.val_type))
          ~local_symbol:(fields_value (optional Symbol.val_type))
          ~security_id_type:(fields_value (optional Raw_contract.Security_id.Type.val_type))
          ~security_id:(fields_value (optional Raw_contract.Security_id.val_type))
          ~order_action:(fields_value (required string))
          ~quantity:(fields_value (required int))
          ~order_kind:(fields_value (required string))
          ~limit_price:(fields_value (optional Price.val_type ~default_on_none:"0.0"))
          ~stop_price:(fields_value (optional Price.val_type ~default_on_none:"0.0"))
          ~time_in_force:(fields_value (optional Raw_order.Time_in_force.val_type))
          ~oca_group_name:(fields_value (optional string))
          ~account_code:(fields_value (required Account_code.val_type))
          ~open_close:(fields_value (required Raw_order.Open_close.val_type))
          ~origin:(fields_value (required Raw_order.Origin.val_type))
          ~order_ref:(fields_value (optional string))
          ~transmit:(fields_value (required bool))
          ~parent_id:(fields_value (optional Raw_order.Id.val_type ~default_on_none:"0"))
          ~block_order:(fields_value (required bool))
          ~sweep_to_fill:(fields_value (required bool))
          ~display_size:(fields_value (optional int ~default_on_none:"0"))
          ~stop_trigger_method:(fields_value (required Raw_order.Stop_trigger_method.val_type))
          ~outside_regular_trading_hours:(fields_value (required bool))
          ~hidden:(fields_value (required bool))
          ~shares_allocation:(fields_value (required string))
          ~discretionary_amount:(fields_value (optional float ~default_on_none:"0.0"))
          ~good_after_date_time:(fields_value (optional time))
          ~good_till_date_time:(fields_value (optional time))
          ~financial_advisor_group:(fields_value (optional string))
          ~financial_advisor_method:(fields_value (optional string))
          ~financial_advisor_percentage:(fields_value (optional string))
          ~financial_advisor_profile:(fields_value (optional string))
          ~short_sale_slot:(fields_value (optional int ~default_on_none:"0"))
          ~designated_location:(fields_value (optional string))
          ~exemption_code:(fields_value (required int))
          ~oca_type:(fields_value (optional Raw_order.Oca_type.val_type ~default_on_none:"0"))
          ~rule80A:(fields_value (optional Raw_order.Rule80A.val_type))
          ~settling_firm:(fields_value (optional string))
          ~all_or_none:(fields_value (required bool))
          ~minimum_quantity:(fields_value (optional int))
          ~percent_offset:(fields_value (optional float))
          ~electronic_trade_only:(fields_value (required bool))
          ~firm_quote_only:(fields_value (required bool))
          ~nbbo_price_cap:(fields_value (optional float))
          ~auction_strategy:(fields_value (optional Raw_order.Auction_strategy.val_type ~default_on_none:"0"))
          ~starting_price:(fields_value (optional Price.val_type))
          ~stock_reference_price:(fields_value (optional Price.val_type))
          ~delta:(fields_value (optional float))
          ~lower_stock_price_range:(fields_value (optional Price.val_type))
          ~upper_stock_price_range:(fields_value (optional Price.val_type))
          ~override_percentage_constraints:(fields_value (required bool))
          ~volatility:(fields_value (optional float))
          ~volatility_type:(fields_value (optional Raw_order.Volatility_type.val_type))
          ~delta_neutral_order_type:(fields_value (optional Raw_order.Type.val_type))
          ~delta_neutral_aux_price:(fields_value (optional Price.val_type))
          ~continuous_update:(fields_value (required bool))
          ~reference_price_type:(fields_value (optional Raw_order.Reference_price_type.val_type))
          ~trailing_stop_price:(fields_value (optional Price.val_type))
          ~trailing_percent:(fields_value (optional float))
          ~scale_initial_level_size:(fields_value (optional int))
          ~scale_subsequent_level_size:(fields_value (optional int))
          ~scale_price_increment:(fields_value (optional float))
          ~hedge_type:(fields_value (optional Raw_order.Hedge_type.val_type))
          ~opt_out_smart_routing:(fields_value (required bool))
          ~clearing_account:(fields_value (optional string))
          ~clearing_intent:(fields_value (optional Raw_order.Clearing_intent.val_type))
          ~not_held:(fields_value (required bool))
          ~underlying_combo:(fields_value (required bool))
          ~algo_strategy:(fields_value (optional string))
          ~request_pre_trade_information:(fields_value (required bool)))
        (fun t ->
          `Args
            $ t.contract_id
            $ t.symbol
            $ t.contract_type
            $ t.expiry
            $ t.strike
            $ t.option_right
            $ t.multiplier
            $ t.exchange
            $ t.listing_exchange
            $ t.currency
            $ t.local_symbol
            $ t.security_id_type
            $ t.security_id
            $ t.order_action
            $ t.quantity
            $ t.order_kind
            $ t.limit_price
            $ t.stop_price
            $ t.time_in_force
            $ t.oca_group_name
            $ t.account_code
            $ t.open_close
            $ t.origin
            $ t.order_ref
            $ t.transmit
            $ t.parent_id
            $ t.block_order
            $ t.sweep_to_fill
            $ t.display_size
            $ t.stop_trigger_method
            $ t.outside_regular_trading_hours
            $ t.hidden
            $ t.shares_allocation
            $ t.discretionary_amount
            $ t.good_after_date_time
            $ t.good_till_date_time
            $ t.financial_advisor_group
            $ t.financial_advisor_method
            $ t.financial_advisor_percentage
            $ t.financial_advisor_profile
            $ t.short_sale_slot
            $ t.designated_location
            $ t.exemption_code
            $ t.oca_type
            $ t.rule80A
            $ t.settling_firm
            $ t.all_or_none
            $ t.minimum_quantity
            $ t.percent_offset
            $ t.electronic_trade_only
            $ t.firm_quote_only
            $ t.nbbo_price_cap
            $ t.auction_strategy
            $ t.starting_price
            $ t.stock_reference_price
            $ t.delta
            $ t.lower_stock_price_range
            $ t.upper_stock_price_range
            $ t.override_percentage_constraints
            $ t.volatility
            $ t.volatility_type
            $ t.delta_neutral_order_type
            $ t.delta_neutral_aux_price
            $ t.continuous_update
            $ t.reference_price_type
            $ t.trailing_stop_price
            $ t.trailing_percent
            $ t.scale_initial_level_size
            $ t.scale_subsequent_level_size
            $ t.scale_price_increment
            $ t.hedge_type
            $ t.opt_out_smart_routing
            $ t.clearing_account
            $ t.clearing_intent
            $ t.not_held
            $ t.underlying_combo
            $ t.algo_strategy
            $ t.request_pre_trade_information)))

let unpickler = Only_in_test.of_thunk (fun () ->
  Unpickler.create ~name:"Submit-order"
    Unpickler.Spec.(
      Fields.fold
        ~init:(empty ())
        ~contract_id:(fields_value (optional Raw_contract.Id.val_type ~none_on_default:"0"))
        ~symbol:(fields_value (required Symbol.val_type))
        ~contract_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type ~none_on_default:"0.0"))
        ~option_right:(fields_value (optional Raw_contract.Option_right.val_type))
        ~multiplier:(fields_value (optional string))
        ~exchange:(fields_value (required Exchange.val_type))
        ~listing_exchange:(fields_value (optional Exchange.val_type))
        ~currency:(fields_value (required Currency.val_type))
        ~local_symbol:(fields_value (optional Symbol.val_type))
        ~security_id_type:(fields_value (optional Raw_contract.Security_id.Type.val_type))
        ~security_id:(fields_value (optional Raw_contract.Security_id.val_type))
        ~order_action:(fields_value (required string))
        ~quantity:(fields_value (required int))
        ~order_kind:(fields_value (required string))
        ~limit_price:(fields_value (optional Price.val_type ~none_on_default:"0.0"))
        ~stop_price:(fields_value (optional Price.val_type ~none_on_default:"0.0"))
        ~time_in_force:(fields_value (optional Raw_order.Time_in_force.val_type))
        ~oca_group_name:(fields_value (optional string))
        ~account_code:(fields_value (required Account_code.val_type))
        ~open_close:(fields_value (required Raw_order.Open_close.val_type))
        ~origin:(fields_value (required Raw_order.Origin.val_type))
        ~order_ref:(fields_value (optional string))
        ~transmit:(fields_value (required bool))
        ~parent_id:(fields_value (optional Raw_order.Id.val_type ~none_on_default:"0"))
        ~block_order:(fields_value (required bool))
        ~sweep_to_fill:(fields_value (required bool))
        ~display_size:(fields_value (optional int ~none_on_default:"0"))
        ~stop_trigger_method:(fields_value (required Raw_order.Stop_trigger_method.val_type))
        ~outside_regular_trading_hours:(fields_value (required bool))
        ~hidden:(fields_value (required bool))
        ~shares_allocation:(fields_value (required string))
        ~discretionary_amount:(fields_value (optional float ~none_on_default:"0.0"))
        ~good_after_date_time:(fields_value (optional time))
        ~good_till_date_time:(fields_value (optional time))
        ~financial_advisor_group:(fields_value (optional string))
        ~financial_advisor_method:(fields_value (optional string))
        ~financial_advisor_percentage:(fields_value (optional string))
        ~financial_advisor_profile:(fields_value (optional string))
        ~short_sale_slot:(fields_value (optional int ~none_on_default:"0"))
        ~designated_location:(fields_value (optional string))
        ~exemption_code:(fields_value (required int))
        ~oca_type:(fields_value (optional Raw_order.Oca_type.val_type ~none_on_default:"0"))
        ~rule80A:(fields_value (optional Raw_order.Rule80A.val_type))
        ~settling_firm:(fields_value (optional string))
        ~all_or_none:(fields_value (required bool))
        ~minimum_quantity:(fields_value (optional int))
        ~percent_offset:(fields_value (optional float))
        ~electronic_trade_only:(fields_value (required bool))
        ~firm_quote_only:(fields_value (required bool))
        ~nbbo_price_cap:(fields_value (optional float))
        ~auction_strategy:(fields_value (optional Raw_order.Auction_strategy.val_type ~none_on_default:"0"))
        ~starting_price:(fields_value (optional Price.val_type))
        ~stock_reference_price:(fields_value (optional Price.val_type))
        ~delta:(fields_value (optional float))
        ~lower_stock_price_range:(fields_value (optional Price.val_type))
        ~upper_stock_price_range:(fields_value (optional Price.val_type))
        ~override_percentage_constraints:(fields_value (required bool))
        ~volatility:(fields_value (optional float))
        ~volatility_type:(fields_value (optional Raw_order.Volatility_type.val_type))
        ~delta_neutral_order_type:(fields_value (optional Raw_order.Type.val_type))
        ~delta_neutral_aux_price:(fields_value (optional Price.val_type))
        ~continuous_update:(fields_value (required bool))
        ~reference_price_type:(fields_value (optional Raw_order.Reference_price_type.val_type))
        ~trailing_stop_price:(fields_value (optional Price.val_type))
        ~trailing_percent:(fields_value (optional float))
        ~scale_initial_level_size:(fields_value (optional int))
        ~scale_subsequent_level_size:(fields_value (optional int))
        ~scale_price_increment:(fields_value (optional float))
        ~hedge_type:(fields_value (optional Raw_order.Hedge_type.val_type))
        ~opt_out_smart_routing:(fields_value (required bool))
        ~clearing_account:(fields_value (optional string))
        ~clearing_intent:(fields_value (optional Raw_order.Clearing_intent.val_type))
        ~not_held:(fields_value (required bool))
        ~underlying_combo:(fields_value (required bool))
        ~algo_strategy:(fields_value (optional string))
        ~request_pre_trade_information:(fields_value (required bool)))
    (fun
      contract_id
      symbol
      contract_type
      expiry
      strike
      option_right
      multiplier
      exchange
      listing_exchange
      currency
      local_symbol
      security_id_type
      security_id
      order_action
      quantity
      order_kind
      limit_price
      stop_price
      time_in_force
      oca_group_name
      account_code
      open_close
      origin
      order_ref
      transmit
      parent_id
      block_order
      sweep_to_fill
      display_size
      stop_trigger_method
      outside_regular_trading_hours
      hidden
      shares_allocation
      discretionary_amount
      good_after_date_time
      good_till_date_time
      financial_advisor_group
      financial_advisor_method
      financial_advisor_percentage
      financial_advisor_profile
      short_sale_slot
      designated_location
      exemption_code
      oca_type
      rule80A
      settling_firm
      all_or_none
      minimum_quantity
      percent_offset
      electronic_trade_only
      firm_quote_only
      nbbo_price_cap
      auction_strategy
      starting_price
      stock_reference_price
      delta
      lower_stock_price_range
      upper_stock_price_range
      override_percentage_constraints
      volatility
      volatility_type
      delta_neutral_order_type
      delta_neutral_aux_price
      continuous_update
      reference_price_type
      trailing_stop_price
      trailing_percent
      scale_initial_level_size
      scale_subsequent_level_size
      scale_price_increment
      hedge_type
      opt_out_smart_routing
      clearing_account
      clearing_intent
      not_held
      underlying_combo
      algo_strategy
      request_pre_trade_information ->
        { contract_id;
          symbol;
          contract_type;
          expiry;
          strike;
          option_right;
          multiplier;
          exchange;
          listing_exchange;
          currency;
          local_symbol;
          security_id_type;
          security_id;
          order_action;
          quantity;
          order_kind;
          limit_price;
          stop_price;
          time_in_force;
          oca_group_name;
          account_code;
          open_close;
          origin;
          order_ref;
          transmit;
          parent_id;
          block_order;
          sweep_to_fill;
          display_size;
          stop_trigger_method;
          outside_regular_trading_hours;
          hidden;
          shares_allocation;
          discretionary_amount;
          good_after_date_time;
          good_till_date_time;
          financial_advisor_group;
          financial_advisor_method;
          financial_advisor_percentage;
          financial_advisor_profile;
          short_sale_slot;
          designated_location;
          exemption_code;
          oca_type;
          rule80A;
          settling_firm;
          all_or_none;
          minimum_quantity;
          percent_offset;
          electronic_trade_only;
          firm_quote_only;
          nbbo_price_cap;
          auction_strategy;
          starting_price;
          stock_reference_price;
          delta;
          lower_stock_price_range;
          upper_stock_price_range;
          override_percentage_constraints;
          volatility;
          volatility_type;
          delta_neutral_order_type;
          delta_neutral_aux_price;
          continuous_update;
          reference_price_type;
          trailing_stop_price;
          trailing_percent;
          scale_initial_level_size;
          scale_subsequent_level_size;
          scale_price_increment;
          hedge_type;
          opt_out_smart_routing;
          clearing_account;
          clearing_intent;
          not_held;
          underlying_combo;
          algo_strategy;
          request_pre_trade_information;
        }))
