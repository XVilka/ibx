(* File: query.ml

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
open Std_internal

module Unit (Arg : sig val name:string end) = struct
  type t = unit with sexp
  let create () = ()
  let ( = ) t1 t2 = (t1 = t2)
  let pickler =
    Pickler.create ~name:Arg.name
      Pickler.Spec.(value (required unit))
  let unpickler = Only_in_test.of_thunk (fun () ->
    Unpickler.create ~name:Arg.name
      Unpickler.Spec.(value (required unit) ~name:"unit")
      Fn.id)
end

(* +-----------------------------------------------------------------------+
   | Connection and server                                                 |
   +-----------------------------------------------------------------------+ *)

module Server_log_level = struct
  module Level = struct
    type t = [ `System | `Error | `Warning | `Information | `Detail ] with sexp

    let tws_of_t = function
      | `System -> "1"
      | `Error -> "2"
      | `Warning -> "3"
      | `Information -> "4"
      | `Detail -> "5"

    let t_of_tws = function
      | "1" -> `System
      | "2" -> `Error
      | "3" -> `Warning
      | "4" -> `Information
      | "5" -> `Detail
      | s   -> invalid_argf "Level.t_of_tws: %s" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t = Level.t with sexp

  let create ~level = level

  let ( = ) t1 t2 = (t1 = t2)

  let pickler =
    Pickler.create ~name:"Query.Server_log_level"
      Pickler.Spec.(value (required Level.val_type))

  let unpickler = Only_in_test.of_thunk (fun () ->
    Unpickler.create ~name:"Query.Server_log_level"
      Unpickler.Spec.(value (required Level.val_type) ~name:"log_level")
      Fn.id)
end

module Server_time = Unit (struct
  let name = "Query.Server_time"
end)

(* +-----------------------------------------------------------------------+
   | Market data                                                           |
   +-----------------------------------------------------------------------+ *)

module Market_data = struct
  module Tick_kind = struct
    type t =
    [ `Option_volume
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
    ] with sexp

    let tws_of_t = function
      | `Option_volume -> "100"
      | `Option_open_interest -> "101"
      | `Historical_volatility -> "104"
      | `Implied_volatility -> "106"
      | `Index_future_premium -> "162"
      | `Misc_stats -> "165"
      | `Mark_price -> "221"
      | `Auction_values -> "225"
      | `Realtime_volume -> "233"
      | `Shortable -> "236"
      | `Inventory -> "256"
      | `Fundamental_ratios -> "258"
      | `Turn_off_market_data -> "mdoff"

    let t_of_tws = function
      | "100" -> `Option_volume
      | "101" -> `Option_open_interest
      | "104" -> `Historical_volatility
      | "106" -> `Implied_volatility
      | "162" -> `Index_future_premium
      | "165" -> `Misc_stats
      | "221" -> `Mark_price
      | "225" -> `Auction_values
      | "233" -> `Realtime_volume
      | "236" -> `Shortable
      | "256" -> `Inventory
      | "258" -> `Fundamental_ratios
      | "mdoff" -> `Turn_off_market_data
      | s   -> invalid_argf "Tick_kind.t_of_tws: %s" s ()
  end

  type t =
    { contract : Raw_contract.t;
      tick_generics : Tick_kind.t list;
      snapshot : bool;
    } with sexp, fields

  let create ~contract ~tick_generics ~snapshot =
    { contract = Contract.to_raw contract;
      tick_generics;
      snapshot;
    }

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~contract:(use Raw_contract.(=))
      ~tick_generics:(use (=))
      ~snapshot:(use (=))

  let pickler =
    let contract_spec =
      Raw_contract.Pickler_specs.market_data_query ()
    in
    Pickler.create ~name:"Query.Market_data"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~tick_generics:(fields_value (required string))
            ~snapshot:(fields_value (required bool)))
          (fun { contract; tick_generics; snapshot } ->
            let tick_generics =
              String.concat (List.map tick_generics ~f:Tick_kind.tws_of_t) ~sep:","
            in
            `Args $ contract $ tick_generics $ snapshot))

  let unpickler = Only_in_test.of_thunk (fun () ->
    let contract_spec =
      Raw_contract.Unpickler_specs.market_data_query ()
    in
    Unpickler.create ~name:"Query.Market_data"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~tick_generics:(fields_value (required string))
          ~snapshot:(fields_value (required bool)))
      (fun contract tick_generics snapshot ->
        let tick_generics = match tick_generics with
          | "" -> []
          | s  -> List.map (String.split s ~on:',') ~f:Tick_kind.t_of_tws
        in
        { contract; tick_generics; snapshot }))
end

module Calc_option_price = struct
  type t =
    { contract : Raw_contract.t;
      volatility : float;
      underlying_price : Price.t;
    } with sexp, fields

  let create ~contract ~volatility ~underlying_price =
    { contract = Contract.to_raw contract;
      volatility;
      underlying_price;
    }

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~contract:(use Raw_contract.(=))
      ~volatility:(use Float.(=.))
      ~underlying_price:(use Price.(=.))

  let pickler =
    let contract_spec =
      Raw_contract.Pickler_specs.common_option_calc ()
    in
    Pickler.create ~name:"Query.Option_price"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~volatility:(fields_value (required float))
            ~underlying_price:(fields_value (required Price.val_type)))
          (fun { contract; volatility; underlying_price } ->
            `Args $ contract $ volatility $ underlying_price))

  let unpickler = Only_in_test.of_thunk (fun () ->
    let contract_spec =
      Raw_contract.Unpickler_specs.option_price_query ()
    in
    Unpickler.create ~name:"Query.Option_price"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~volatility:(fields_value (required float))
          ~underlying_price:(fields_value (required Price.val_type)))
      (fun contract volatility underlying_price ->
        { contract; volatility; underlying_price }))
end

module Calc_implied_volatility = struct
  type t =
    { contract : Raw_contract.t;
      option_price : Price.t;
      underlying_price : Price.t;
    } with sexp, fields

  let create ~contract ~option_price ~underlying_price =
    { contract = Contract.to_raw contract;
      option_price;
      underlying_price;
    }

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~contract:(use Raw_contract.(=))
      ~option_price:(use Price.(=.))
      ~underlying_price:(use Price.(=.))

  let pickler =
    let contract_spec =
      Raw_contract.Pickler_specs.common_option_calc ()
    in
    Pickler.create ~name:"Query.Implied_volatility"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~option_price:(fields_value (required Price.val_type))
            ~underlying_price:(fields_value (required Price.val_type)))
          (fun { contract; option_price; underlying_price } ->
            `Args $ contract $ option_price $ underlying_price))

  let unpickler = Only_in_test.of_thunk (fun () ->
    let contract_spec =
      Raw_contract.Unpickler_specs.implied_volatility_query ()
    in
    Unpickler.create ~name:"Query.Implied_volatility"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~option_price:(fields_value (required Price.val_type))
          ~underlying_price:(fields_value (required Price.val_type)))
      (fun contract option_price underlying_price ->
        { contract; option_price; underlying_price }))
end


(* +-----------------------------------------------------------------------+
   | Orders                                                                |
   +-----------------------------------------------------------------------+ *)

module Submit_order = Submit_order

(* +-----------------------------------------------------------------------+
   | Account and portfolio                                                 |
   +-----------------------------------------------------------------------+ *)

module Updates (Arg : sig val name:string end) = struct
  type t =
    { subscribe : bool;
      account_code : Account_code.t;
    } with sexp, fields

  let create = Fields.create

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~subscribe:(use (=))
      ~account_code:(use Account_code.(=))

  let pickler =
    Pickler.create ~name:Arg.name
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~subscribe:(fields_value (required bool))
            ~account_code:(fields_value (required Account_code.val_type)))
      (fun t -> `Args $ t.subscribe $ t.account_code))

  let unpickler = Only_in_test.of_thunk (fun () ->
    Unpickler.create ~name:Arg.name
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~subscribe:(fields_value (required bool))
          ~account_code:(fields_value (required Account_code.val_type)))
      (fun subscribe account_code -> { subscribe; account_code }))
end

module Account_updates = Updates (struct
  let name = "Query.Account_updates"
end)

module Positions = Updates (struct
  let name = "Query.Positions"
end)

(* +-----------------------------------------------------------------------+
   | Executions                                                            |
   +-----------------------------------------------------------------------+ *)

module Executions = struct
  module Time = struct
    include Time
    let tws_of_t tm = Time.format tm "%Y%m%d-%H:%M:%S"
    let t_of_tws s = Time.of_string (String.tr ~target:'-' ~replacement:' ' s)
    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t =
    { client_id : Client_id.t;
      account_code : Account_code.t;
      time : Time.t;
      symbol : Symbol.t;
      sec_type : string;
      exchange : Exchange.t;
      order_action : Order.Action.t;
    } with sexp, fields

  let create ~contract ~client_id ~account_code ~time ~order_action =
    let contract = Contract.to_raw contract in
    { client_id;
      account_code;
      time;
      symbol = Raw_contract.symbol contract;
      sec_type = Raw_contract.sec_type contract;
      exchange = Raw_contract.exchange contract;
      order_action;
    }

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~client_id:(use Client_id.(=))
      ~account_code:(use Account_code.(=))
      ~time:(use Time.(=))
      ~symbol:(use Symbol.(=))
      ~sec_type:(use (=))
      ~exchange:(use (=))
      ~order_action:(use (=))

  let pickler =
    Pickler.create ~name:"Query.Executions"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~client_id:(fields_value (required Client_id.val_type))
            ~account_code:(fields_value (required Account_code.val_type))
            ~time:(fields_value (required Time.val_type))
            ~symbol:(fields_value (required Symbol.val_type))
            ~sec_type:(fields_value (required string))
            ~exchange:(fields_value (required Exchange.val_type))
            ~order_action:(fields_value (required Raw_order.Action.val_type)))
          (fun t ->
            `Args
              $ t.client_id
              $ t.account_code
              $ t.time
              $ t.symbol
              $ t.sec_type
              $ t.exchange
              $ t.order_action))

  let unpickler = Only_in_test.of_thunk (fun () ->
    Unpickler.create ~name:"Query.Executions"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~client_id:(fields_value (required Client_id.val_type))
          ~account_code:(fields_value (required Account_code.val_type))
          ~time:(fields_value (required Time.val_type))
          ~symbol:(fields_value (required Symbol.val_type))
          ~sec_type:(fields_value (required string))
          ~exchange:(fields_value (required Exchange.val_type))
          ~order_action:(fields_value (required Raw_order.Action.val_type)))
      (fun client_id account_code time symbol sec_type exchange order_action ->
        { client_id;
          account_code;
          time;
          symbol;
          sec_type;
          exchange;
          order_action;
        }))
end

(* +-----------------------------------------------------------------------+
   | Contract details                                                      |
   +-----------------------------------------------------------------------+ *)

module Contract_details = struct
  type t = Raw_contract.t with sexp

  let create ~contract = Contract.to_raw contract

  let create ?con_id ?multiplier ?listing_exchange ?local_symbol ?sec_id
      ?include_expired ?exchange ?option_right ?expiry ?strike ~sec_type
      ~currency symbol =
    Raw_contract.create
      ?con_id
      ?multiplier
      ?listing_exchange
      ?local_symbol
      ?sec_id_type:(Option.map sec_id ~f:Security_id.sec_id_type)
      ?sec_id:(Option.map sec_id ~f:Security_id.sec_id)
      ?exchange
      ?expiry
      ?strike
      ?option_right
      ~currency
      ~sec_type:(Security_type.tws_of_t sec_type)
      symbol

  let ( = ) t1 t2 = Raw_contract.(=) t1 t2

  let pickler =
    Pickler.create ~name:"Query.Contract_details"
      (Raw_contract.Pickler_specs.contract_details_query ())

  let unpickler = Only_in_test.of_thunk (fun () ->
    Unpickler.create ~name:"Query.Contract_details"
      (Raw_contract.Unpickler_specs.contract_details_query ())
      Fn.id)
end

(* +-----------------------------------------------------------------------+
   | Market depth                                                          |
   +-----------------------------------------------------------------------+ *)

module Market_depth = struct
  type t =
    { contract : Raw_contract.t;
      num_rows : int;
    } with sexp, fields

  let create ~contract ~num_rows =
    { contract = Contract.to_raw contract;
      num_rows;
    }

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~contract:(use Raw_contract.(=))
      ~num_rows:(use (=))

  let pickler =
    let contract_spec =
      Raw_contract.Pickler_specs.market_depth_query ()
    in
    Pickler.create ~name:"Query.Market_depth"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~num_rows:(fields_value (required int)))
          (fun { contract; num_rows } -> `Args $ contract $ num_rows ))

  let unpickler = Only_in_test.of_thunk (fun () ->
    let contract_spec =
      Raw_contract.Unpickler_specs.market_depth_query ()
    in
    Unpickler.create ~name:"Query.Market_depth"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~num_rows:(fields_value (required int)))
      (fun contract num_rows -> { contract; num_rows }))
end

(* +-----------------------------------------------------------------------+
   | Historical data                                                       |
   +-----------------------------------------------------------------------+ *)

module History = struct
  module Bar_size = struct
    module T = struct
      type t =
      [ `One_sec | `Five_secs | `Fifteen_secs | `Thirty_secs
      | `One_min | `Two_mins | `Three_mins | `Five_mins
      | `Fifteen_mins | `Thirty_mins
      | `One_hour
      | `One_day
      ] with sexp
    end
    include T
    include Sexpable.To_stringable (T)

    let tws_of_t = function
      | `One_sec -> "1 sec"
      | `Five_secs -> "5 secs"
      | `Fifteen_secs -> "15 secs"
      | `Thirty_secs -> "30 secs"
      | `One_min -> "1 min"
      | `Two_mins -> "2 mins"
      | `Three_mins -> "3 mins"
      | `Five_mins -> "5 mins"
      | `Fifteen_mins -> "15 mins"
      | `Thirty_mins -> "30 mins"
      | `One_hour -> "1 hour"
      | `One_day -> "1 day"

    let t_of_tws = function
      | "1 sec" -> `One_sec
      | "5 secs" -> `Five_secs
      | "15 secs" -> `Fifteen_secs
      | "30 secs" -> `Thirty_secs
      | "1 min" -> `One_min
      | "2 mins" -> `Two_mins
      | "3 mins" -> `Three_mins
      | "5 mins" -> `Five_mins
      | "15 mins" -> `Fifteen_mins
      | "30 mins" -> `Thirty_mins
      | "1 hour" -> `One_hour
      | "1 day" -> `One_day
      | s -> invalid_argf "Bar_size.t_of_tws: %S" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  module Bar_span = struct
    module T = struct
      type t =
      [ `Sec of int
      | `Day of int
      | `Week of int
      | `Month of int
      | `Year of int
      ] with sexp
    end
    include T
    include Sexpable.To_stringable (T)

    let tws_of_t = function
      | `Sec   x -> sprintf "%d S" x
      | `Day   x -> sprintf "%d D" x
      | `Week  x -> sprintf "%d W" x
      | `Month x -> sprintf "%d M" x
      | `Year  x -> sprintf "%d Y" x

    let t_of_tws s =
      let extract_int s ~time_unit =
        let pattern = "\\([1-9][0-9]*\\)  " in
        String.nset pattern 16 time_unit;
        if Str.string_match (Str.regexp pattern) s 0 then
          Int.of_string (Str.matched_group 1 s)
        else invalid_argf "Bar_span.t_of_tws: %S" s ()
      in
      match String.nget s (String.length s - 1) with
      | 'S' -> `Sec   (extract_int s ~time_unit:'S')
      | 'D' -> `Day   (extract_int s ~time_unit:'D')
      | 'W' -> `Week  (extract_int s ~time_unit:'W')
      | 'M' -> `Month (extract_int s ~time_unit:'M')
      | 'Y' -> `Year  (extract_int s ~time_unit:'Y')
      | _ -> invalid_argf "Bar_span.t_of_tws: %S" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  module Tick_type = struct
    module T = struct
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
    end
    include T
    include Sexpable.To_stringable (T)

    let tws_of_t = function
      | `Trades -> "TRADES"
      | `Midpoint -> "MIDPOINT"
      | `Bid -> "BID"
      | `Ask -> "ASK"
      | `Bid_ask -> "BID_ASK"
      | `Historical_volatility -> "HISTORICAL_VOLATILITY"
      | `Implied_volatility -> "OPTION_IMPLIED_VOLATILITY"
      | `Option_volume -> "OPTION_VOLUME"

    let t_of_tws = function
      | "TRADES" -> `Trades
      | "MIDPOINT" -> `Midpoint
      | "BID" -> `Bid
      | "ASK" -> `Ask
      | "BID_ASK" -> `Bid_ask
      | "HISTORICAL_VOLATILITY" -> `Historical_volatility
      | "OPTION_IMPLIED_VOLATILITY" -> `Implied_volatility
      | "OPTION_VOLUME" -> `Option_volume
      | s -> invalid_argf "Tick_type.t_of_tws: %S" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t =
    { contract : Raw_contract.t;
      until : Time.t;
      bar_size : Bar_size.t;
      bar_span : Bar_span.t;
      use_tradehours : bool;
      tick_type : Tick_type.t;
      date_format : string;
    } with sexp, fields

  let create ~contract ~until ~bar_size ~bar_span ~use_tradehours ~tick_type =
    { contract = Contract.to_raw contract;
      until;
      bar_size;
      bar_span;
      use_tradehours;
      tick_type;
      date_format = "1";
    }

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~contract:(use Raw_contract.(=))
      ~until:(use Time.(=))
      ~bar_size:(use (=))
      ~bar_span:(use (=))
      ~use_tradehours:(use (=))
      ~tick_type:(use (=))
      ~date_format:(use (=))

  let pickler =
    let contract_spec = Raw_contract.Pickler_specs.history_query () in
    Pickler.create ~name:"Query.History"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~until:(fields_value (required time))
            ~bar_size:(fields_value (required Bar_size.val_type))
            ~bar_span:(fields_value (required Bar_span.val_type))
            ~use_tradehours:(fields_value (required bool))
            ~tick_type:(fields_value (required Tick_type.val_type))
            ~date_format:(fields_value (required string)))
          (fun t ->
            `Args
              $ t.contract
              $ t.until
              $ t.bar_size
              $ t.bar_span
              $ t.use_tradehours
              $ t.tick_type
              $ t.date_format))

  let unpickler = Only_in_test.of_thunk (fun () ->
    let contract_spec = Raw_contract.Unpickler_specs.history_query () in
    Unpickler.create ~name:"Query.History"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~until:(fields_value (required time))
          ~bar_size:(fields_value (required Bar_size.val_type))
          ~bar_span:(fields_value (required Bar_span.val_type))
          ~use_tradehours:(fields_value (required bool))
          ~tick_type:(fields_value (required Tick_type.val_type))
          ~date_format:(fields_value (required string)))
      (fun contract until bar_size bar_span use_tradehours tick_type date_format ->
        { contract;
          until;
          bar_size;
          bar_span;
          use_tradehours;
          tick_type;
          date_format;
        }))
end

(* +-----------------------------------------------------------------------+
   | Realtime bars                                                         |
   +-----------------------------------------------------------------------+ *)

module Realtime_bars = struct
  module Bar_size = struct
    type t = [ `Five_secs ] with sexp

    let tws_of_t = function
      | `Five_secs -> "5"

    let t_of_tws = function
      | "5" -> `Five_secs
      | s -> invalid_argf "Bar_size.t_of_tws: %S" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  module Tick_type = struct
    type t = [ `Trades | `Midpoint | `Bid | `Ask ] with sexp

    let tws_of_t = function
      | `Trades -> "TRADES"
      | `Bid -> "BID"
      | `Ask -> "ASK"
      | `Midpoint -> "MIDPOINT"

    let t_of_tws = function
      | "TRADES" -> `Trades
      | "BID" -> `Bid
      | "ASK" -> `Ask
      | "MIDPOINT" -> `Midpoint
      | s -> invalid_argf "Tick_type.t_of_tws: %s" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t =
    { contract : Raw_contract.t;
      bar_size : Bar_size.t;
      tick_type : Tick_type.t;
      use_tradehours : bool;
    } with sexp, fields

  let create ~contract ~bar_size ~tick_type ~use_tradehours =
    { contract = Contract.to_raw contract;
      bar_size;
      tick_type;
      use_tradehours;
    }

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~contract:(use Raw_contract.(=))
      ~bar_size:(use (=))
      ~tick_type:(use (=))
      ~use_tradehours:(use (=))

  let pickler =
    let contract_spec =
      Raw_contract.Pickler_specs.realtime_bars_query ()
    in
    Pickler.create ~name:"Query.Realtime_bars"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~bar_size:(fields_value (required Bar_size.val_type))
            ~tick_type:(fields_value (required Tick_type.val_type))
            ~use_tradehours:(fields_value (required bool)))
          (fun { contract; bar_size; tick_type; use_tradehours } ->
            `Args $ contract $ bar_size $ tick_type $ use_tradehours ))

  let unpickler = Only_in_test.of_thunk (fun () ->
    let contract_spec =
      Raw_contract.Unpickler_specs.realtime_bars_query ()
    in
    Unpickler.create ~name:"Query.Realtime_bars"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~bar_size:(fields_value (required Bar_size.val_type))
          ~tick_type:(fields_value (required Tick_type.val_type))
          ~use_tradehours:(fields_value (required bool)))
      (fun contract bar_size tick_type use_tradehours ->
        { contract; bar_size; tick_type; use_tradehours }))
end
