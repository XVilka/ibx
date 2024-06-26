open Core
open Tws_prot

type t =
  { (* ===================== contract fields ==================== *)
    con_id : Contract_id.t option;
    symbol : Symbol.t;
    sec_type : string;
    expiry : Date.t option;
    strike : Price.t option;
    right : Option_right.t option;
    multiplier : int option;
    exchange : Exchange.t;
    prim_exch : Exchange.t option;
    currency : Currency.t;
    local_symbol : Symbol.t option;
    sec_id_type : Security_id.Type.t option;
    sec_id : Security_id.Id.t option;
    (* ====================== order fields ====================== *)
    action : Order_action.t;
    quantity : Volume.t;
    order_kind : Order_type.t;
    limit_price : Price.t option;
    stop_price : Price.t option;
    time_in_force : Raw_order.Time_in_force.t option;
    oca_group_name : string option;
    account_code : Account_code.t;
    open_close : Raw_order.Open_close.t;
    origin : Raw_order.Origin.t;
    order_ref : string option;
    transmit : bool;
    parent_id : Order_id.t option;
    block_order : bool;
    sweep_to_fill : bool;
    display_size : Volume.t option;
    stop_trigger_method : Raw_order.Stop_trigger_method.t;
    outside_regular_trading_hours : bool;
    hidden : bool;
    shares_allocation : string;
    discretionary_amount : float option;
    good_after_date_time : Time_float_unix.t option;
    good_till_date_time : Time_float_unix.t option;
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
    minimum_quantity : Volume.t option;
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
    delta_neutral_order_type : Order_type.t option;
    delta_neutral_aux_price : Price.t option;
    continuous_update : bool;
    reference_price_type : Raw_order.Reference_price_type.t option;
    trailing_stop_price : Price.t option;
    trailing_percent : float option;
    scale_initial_level_size : Volume.t option;
    scale_subsequent_level_size : Volume.t option;
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
[@@deriving sexp, fields]

let create ~contract ~order ~account_code =
  let contract = Contract.to_raw contract in
  let order = Order.to_raw order in
  { (* ========================== contract fields =========================== *)
    con_id                          = contract.Raw_contract.con_id;
    symbol                          = contract.Raw_contract.symbol;
    sec_type                        = contract.Raw_contract.sec_type;
    expiry                          = contract.Raw_contract.expiry;
    strike                          = contract.Raw_contract.strike;
    right                           = contract.Raw_contract.right;
    multiplier                      = contract.Raw_contract.multiplier;
    exchange                        = contract.Raw_contract.exchange;
    prim_exch                       = contract.Raw_contract.prim_exch;
    currency                        = contract.Raw_contract.currency;
    local_symbol                    = contract.Raw_contract.local_symbol;
    sec_id_type                     = contract.Raw_contract.sec_id_type;
    sec_id                          = contract.Raw_contract.sec_id;
    (* =========================== order fields ============================= *)
    action                          = order.Raw_order.action;
    quantity                        = order.Raw_order.quantity;
    order_kind                      = order.Raw_order.order_type;
    limit_price                     = order.Raw_order.limit_price;
    stop_price                      = order.Raw_order.stop_price;
    time_in_force                   = order.Raw_order.time_in_force;
    oca_group_name                  = order.Raw_order.oca_group_name;
    account_code;  (* The account code of the current session. *)
    open_close                      = order.Raw_order.open_close;
    origin                          = order.Raw_order.origin;
    order_ref                       = order.Raw_order.order_ref;
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
    ~con_id:(use (Option.equal Contract_id.(=)))
    ~symbol:(use Symbol.(=))
    ~sec_type:(use String.(=))
    ~expiry:(use (Option.equal Date.equal))
    ~strike:(use (Option.equal Price.(=.)))
    ~right:(use (Option.equal Option_right.equal))
    ~multiplier:(use (Option.equal (=)))
    ~exchange:(use Exchange.equal)
    ~prim_exch:(use (Option.equal Exchange.equal))
    ~currency:(use (Currency.equal))
    ~local_symbol:(use (Option.equal Symbol.equal))
    ~sec_id_type:(use (Option.equal Security_id.Type.equal))
    ~sec_id:(use (Option.equal Security_id.Id.equal))
    ~action:(use Order_action.equal)
    ~quantity:(use Volume.equal)
    ~order_kind:(use Order_type.equal)
    ~limit_price:(use (Option.equal Price.(=.)))
    ~stop_price:(use (Option.equal Price.(=.)))
    ~time_in_force:(use (Option.equal Raw_order.Time_in_force.equal))
    ~oca_group_name:(use (Option.equal String.(=)))
    ~account_code:(use Account_code.(=))
    ~open_close:(use Raw_order.Open_close.equal)
    ~origin:(use Raw_order.Origin.equal)
    ~order_ref:(use (Option.equal String.(=)))
    ~transmit:(use Bool.(=))
    ~parent_id:(use (Option.equal Order_id.(=)))
    ~block_order:(use Bool.(=))
    ~sweep_to_fill:(use Bool.(=))
    ~display_size:(use (Option.equal Volume.equal))
    ~stop_trigger_method:(use Raw_order.Stop_trigger_method.equal)
    ~outside_regular_trading_hours:(use Bool.(=))
    ~hidden:(use Bool.(=))
    ~shares_allocation:(use String.(=))
    ~discretionary_amount:(use (Option.equal Float.(=.)))
    ~good_after_date_time:(use (Option.equal Time_float_unix.(=)))
    ~good_till_date_time:(use (Option.equal Time_float_unix.(=)))
    ~financial_advisor_group:(use (Option.equal String.(=)))
    ~financial_advisor_method:(use (Option.equal String.(=)))
    ~financial_advisor_percentage:(use (Option.equal String.(=)))
    ~financial_advisor_profile:(use (Option.equal String.(=)))
    ~short_sale_slot:(use (Option.equal (=)))
    ~designated_location:(use (Option.equal String.(=)))
    ~exemption_code:(use (=))
    ~oca_type:(use (Option.equal Raw_order.Oca_type.equal))
    ~rule80A:(use (Option.equal Raw_order.Rule80A.equal))
    ~settling_firm:(use (Option.equal String.(=)))
    ~all_or_none:(use Bool.(=))
    ~minimum_quantity:(use (Option.equal Volume.equal))
    ~percent_offset:(use (Option.equal Float.(=.)))
    ~electronic_trade_only:(use Bool.(=))
    ~firm_quote_only:(use Bool.(=))
    ~nbbo_price_cap:(use (Option.equal Float.(=.)))
    ~auction_strategy:(use (Option.equal Raw_order.Auction_strategy.equal))
    ~starting_price:(use (Option.equal Price.(=.)))
    ~stock_reference_price:(use (Option.equal Price.(=.)))
    ~delta:(use (Option.equal Float.(=.)))
    ~lower_stock_price_range:(use (Option.equal Price.(=.)))
    ~upper_stock_price_range:(use (Option.equal Price.(=.)))
    ~override_percentage_constraints:(use Bool.(=))
    ~volatility:(use (Option.equal Float.(=.)))
    ~volatility_type:(use (Option.equal Raw_order.Volatility_type.equal))
    ~delta_neutral_order_type:(use (Option.equal Order_type.equal))
    ~delta_neutral_aux_price:(use (Option.equal Price.(=.)))
    ~continuous_update:(use Bool.(=))
    ~reference_price_type:(use (Option.equal Raw_order.Reference_price_type.equal))
    ~trailing_stop_price:(use (Option.equal Price.(=.)))
    ~trailing_percent:(use (Option.equal Float.(=.)))
    ~scale_initial_level_size:(use (Option.equal Volume.equal))
    ~scale_subsequent_level_size:(use (Option.equal Volume.equal))
    ~scale_price_increment:(use (Option.equal Float.(=.)))
    ~hedge_type:(use (Option.equal Raw_order.Hedge_type.equal))
    ~opt_out_smart_routing:(use Bool.(=))
    ~clearing_account:(use (Option.equal String.(=)))
    ~clearing_intent:(use (Option.equal Raw_order.Clearing_intent.equal))
    ~not_held:(use Bool.(=))
    ~underlying_combo:(use Bool.(=))
    ~algo_strategy:(use (Option.equal String.(=)))
    ~request_pre_trade_information:(use Bool.(=))

let encoder =
  Encoder.create ~name:"Query.Submit_order"
    Encoder.Spec.(
      lift (
        Fields.fold
          ~init:(empty ())
          ~con_id:(fields_value (optional Contract_id.val_type ~default_on_none:"0"))
          ~symbol:(fields_value (required Symbol.val_type))
          ~sec_type:(fields_value (required string))
          ~expiry:(fields_value (optional date))
          ~strike:(fields_value (optional Price.val_type ~default_on_none:"0.0"))
          ~right:(fields_value (optional Option_right.val_type))
          ~multiplier:(fields_value (optional int))
          ~exchange:(fields_value (required Exchange.val_type))
          ~prim_exch:(fields_value (optional Exchange.val_type))
          ~currency:(fields_value (required Currency.val_type))
          ~local_symbol:(fields_value (optional Symbol.val_type))
          ~sec_id_type:(fields_value (optional Security_id.Type.val_type))
          ~sec_id:(fields_value (optional Security_id.Id.val_type))
          ~action:(fields_value (required Order_action.val_type))
          ~quantity:(fields_value (required Volume.val_type))
          ~order_kind:(fields_value (required Order_type.val_type))
          ~limit_price:(fields_value (optional Price.val_type ~default_on_none:"0.0"))
          ~stop_price:(fields_value (optional Price.val_type ~default_on_none:"0.0"))
          ~time_in_force:(fields_value (optional Raw_order.Time_in_force.val_type))
          ~oca_group_name:(fields_value (optional string))
          ~account_code:(fields_value (required Account_code.val_type))
          ~open_close:(fields_value (required Raw_order.Open_close.val_type))
          ~origin:(fields_value (required Raw_order.Origin.val_type))
          ~order_ref:(fields_value (optional string))
          ~transmit:(fields_value (required bool))
          ~parent_id:(fields_value (optional Order_id.val_type ~default_on_none:"0"))
          ~block_order:(fields_value (required bool))
          ~sweep_to_fill:(fields_value (required bool))
          ~display_size:(fields_value (optional Volume.val_type ~default_on_none:"0"))
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
          ~minimum_quantity:(fields_value (optional Volume.val_type))
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
          ~delta_neutral_order_type:(fields_value (optional Order_type.val_type))
          ~delta_neutral_aux_price:(fields_value (optional Price.val_type))
          ~continuous_update:(fields_value (required bool))
          ~reference_price_type:(fields_value (optional Raw_order.Reference_price_type.val_type))
          ~trailing_stop_price:(fields_value (optional Price.val_type))
          ~trailing_percent:(fields_value (optional float))
          ~scale_initial_level_size:(fields_value (optional Volume.val_type))
          ~scale_subsequent_level_size:(fields_value (optional Volume.val_type))
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
           $ t.con_id
           $ t.symbol
           $ t.sec_type
           $ t.expiry
           $ t.strike
           $ t.right
           $ t.multiplier
           $ t.exchange
           $ t.prim_exch
           $ t.currency
           $ t.local_symbol
           $ t.sec_id_type
           $ t.sec_id
           $ t.action
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
           $ t.request_pre_trade_information))

let decoder = Only_in_test.of_thunk (fun () ->
  Decoder.create ~name:"Query.Submit_order"
    Decoder.Spec.(
      Fields.fold
        ~init:(empty ())
        ~con_id:(fields_value (optional Contract_id.val_type ~none_on_default:"0"))
        ~symbol:(fields_value (required Symbol.val_type))
        ~sec_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type ~none_on_default:"0.0"))
        ~right:(fields_value (optional Option_right.val_type))
        ~multiplier:(fields_value (optional int))
        ~exchange:(fields_value (required Exchange.val_type))
        ~prim_exch:(fields_value (optional Exchange.val_type))
        ~currency:(fields_value (required Currency.val_type))
        ~local_symbol:(fields_value (optional Symbol.val_type))
        ~sec_id_type:(fields_value (optional Security_id.Type.val_type))
        ~sec_id:(fields_value (optional Security_id.Id.val_type))
        ~action:(fields_value (required Order_action.val_type))
        ~quantity:(fields_value (required Volume.val_type))
        ~order_kind:(fields_value (required Order_type.val_type))
        ~limit_price:(fields_value (optional Price.val_type ~none_on_default:"0.0"))
        ~stop_price:(fields_value (optional Price.val_type ~none_on_default:"0.0"))
        ~time_in_force:(fields_value (optional Raw_order.Time_in_force.val_type))
        ~oca_group_name:(fields_value (optional string))
        ~account_code:(fields_value (required Account_code.val_type))
        ~open_close:(fields_value (required Raw_order.Open_close.val_type))
        ~origin:(fields_value (required Raw_order.Origin.val_type))
        ~order_ref:(fields_value (optional string))
        ~transmit:(fields_value (required bool))
        ~parent_id:(fields_value (optional Order_id.val_type ~none_on_default:"0"))
        ~block_order:(fields_value (required bool))
        ~sweep_to_fill:(fields_value (required bool))
        ~display_size:(fields_value (optional Volume.val_type ~none_on_default:"0"))
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
        ~minimum_quantity:(fields_value (optional Volume.val_type))
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
        ~delta_neutral_order_type:(fields_value (optional Order_type.val_type))
        ~delta_neutral_aux_price:(fields_value (optional Price.val_type))
        ~continuous_update:(fields_value (required bool))
        ~reference_price_type:(fields_value (optional Raw_order.Reference_price_type.val_type))
        ~trailing_stop_price:(fields_value (optional Price.val_type))
        ~trailing_percent:(fields_value (optional float))
        ~scale_initial_level_size:(fields_value (optional Volume.val_type))
        ~scale_subsequent_level_size:(fields_value (optional Volume.val_type))
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
      con_id
      symbol
      sec_type
      expiry
      strike
      right
      multiplier
      exchange
      prim_exch
      currency
      local_symbol
      sec_id_type
      sec_id
      action
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
      { con_id;
        symbol;
        sec_type;
        expiry;
        strike;
        right;
        multiplier;
        exchange;
        prim_exch;
        currency;
        local_symbol;
        sec_id_type;
        sec_id;
        action;
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
