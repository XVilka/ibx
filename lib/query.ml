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

let wrap_contract_spec contract_spec =
  Pickler.Spec.(
    wrap contract_spec
      (fun contract ->
        `Args
          $ contract.Raw_contract.contract_id
          $ contract.Raw_contract.symbol
          $ contract.Raw_contract.contract_type
          $ contract.Raw_contract.expiry
          $ contract.Raw_contract.strike
          $ contract.Raw_contract.option_right
          $ contract.Raw_contract.multiplier
          $ contract.Raw_contract.exchange
          $ contract.Raw_contract.listing_exchange
          $ contract.Raw_contract.currency
          $ contract.Raw_contract.local_symbol
          $ contract.Raw_contract.include_expired
          $ contract.Raw_contract.security_id_type
          $ contract.Raw_contract.security_id
          $ contract.Raw_contract.combo_legs))

let field_name field = Fieldslib.Field.name field

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
    Pickler.create ~name:"Server_log_level"
      Pickler.Spec.(value (required Level.val_type))

  let unpickler = Only_in_test.of_thunk (fun () ->
    Unpickler.create ~name:"Server_log_level"
      Unpickler.Spec.(value (required Level.val_type) ~name:"log_level")
      Fn.id)
end

module Server_time = Unit (struct
  let name = "Server_time"
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
      Pickler.Spec.(
        Raw_contract.Fields.fold
          ~init:(empty ())
          ~contract_id:(fields_value (optional Raw_contract.Id.val_type))
          ~symbol:(fields_value (required Symbol.val_type))
          ~contract_type:(fields_value (required string))
          ~expiry:(fields_value (optional date))
          ~strike:(fields_value (optional Price.val_type))
          ~option_right:(fields_value (optional Raw_contract.Option_right.val_type))
          ~multiplier:(fields_value (optional string))
          ~exchange:(fields_value (required Exchange.val_type))
          ~listing_exchange:(fields_value (optional Exchange.val_type))
          ~currency:(fields_value (required Currency.val_type))
          ~local_symbol:(fields_value (optional Symbol.val_type))
          ~include_expired:(fields_value skipped)
          ~security_id_type:(fields_value skipped)
          ~security_id:(fields_value skipped)
          ~combo_legs:(fields_value (required int)))
      |> wrap_contract_spec
    in
    Pickler.create ~name:"Market_data"
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
  ;;

  let unpickler = Only_in_test.of_thunk (fun () ->
    let contract_spec =
      Unpickler.Spec.(
        step (fun conv id symbol contract_type expiry strike option_right
          multiplier exchange listing_exchange currency local_symbol
          _combo_legs ->
            let contract = Raw_contract.create
              ?id ~contract_type ?expiry ?strike ?option_right ?multiplier
              ~exchange ?listing_exchange ~currency ?local_symbol symbol
            in
            conv contract
        )
        ++ value (optional Raw_contract.Id.val_type)
          ~name:(field_name Raw_contract.Fields.contract_id)
        ++ value (required Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.symbol)
        ++ value (required string)
          ~name:(field_name Raw_contract.Fields.contract_type)
        ++ value (optional date)
          ~name:(field_name Raw_contract.Fields.expiry)
        ++ value (optional Price.val_type)
          ~name:(field_name Raw_contract.Fields.strike)
        ++ value (optional Raw_contract.Option_right.val_type)
          ~name:(field_name Raw_contract.Fields.option_right)
        ++ value (optional string)
          ~name:(field_name Raw_contract.Fields.multiplier)
        ++ value (required Exchange.val_type)
          ~name:(field_name Raw_contract.Fields.exchange)
        ++ value (optional Exchange.val_type)
          ~name:(field_name Raw_contract.Fields.listing_exchange)
        ++ value (required Currency.val_type)
          ~name:(field_name Raw_contract.Fields.currency)
        ++ value (optional Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.local_symbol)
        ++ value (required int)
          ~name:(field_name Raw_contract.Fields.combo_legs))
    in
    Unpickler.create ~name:"Market_data"
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

module Common_option_calc = struct
  module Pickler = struct
    let contract_spec =
      Pickler.Spec.(
        Raw_contract.Fields.fold
          ~init:(empty ())
          ~contract_id:(fields_value (optional Raw_contract.Id.val_type))
          ~symbol:(fields_value (required Symbol.val_type))
          ~contract_type:(fields_value (required string))
          ~expiry:(fields_value (optional date))
          ~strike:(fields_value (optional Price.val_type))
          ~option_right:(fields_value (optional Raw_contract.Option_right.val_type))
          ~multiplier:(fields_value (optional string))
          ~exchange:(fields_value (required Exchange.val_type))
          ~listing_exchange:(fields_value (optional Exchange.val_type))
          ~currency:(fields_value (required Currency.val_type))
          ~local_symbol:(fields_value (optional Symbol.val_type))
          ~include_expired:(fields_value skipped)
          ~security_id_type:(fields_value skipped)
          ~security_id:(fields_value skipped)
          ~combo_legs:(fields_value skipped))
      |> wrap_contract_spec
  end
end

module Option_price = struct
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
    let contract_spec = Common_option_calc.Pickler.contract_spec in
    Pickler.create ~name:"Option_price"
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
      Unpickler.Spec.(
        step (fun conv id symbol contract_type expiry strike option_right
          multiplier exchange listing_exchange currency local_symbol ->
            let contract = Raw_contract.create
              ?id ~expiry ~strike ~option_right ?multiplier ~exchange
              ?listing_exchange ~currency ?local_symbol ~contract_type
              symbol
            in
            conv contract
        )
        ++ value (optional Raw_contract.Id.val_type)
          ~name:(field_name Raw_contract.Fields.contract_id)
        ++ value (required Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.symbol)
        ++ value (required string)
          ~name:(field_name Raw_contract.Fields.contract_type)
        ++ value (required date)
          ~name:(field_name Raw_contract.Fields.expiry)
        ++ value (required Price.val_type)
          ~name:(field_name Raw_contract.Fields.strike)
        ++ value (required Raw_contract.Option_right.val_type)
          ~name:(field_name Raw_contract.Fields.option_right)
        ++ value (optional string)
          ~name:(field_name Raw_contract.Fields.multiplier)
        ++ value (required Exchange.val_type)
          ~name:(field_name Raw_contract.Fields.exchange)
        ++ value (optional Exchange.val_type)
          ~name:(field_name Raw_contract.Fields.listing_exchange)
        ++ value (required Currency.val_type)
          ~name:(field_name Raw_contract.Fields.currency)
        ++ value (optional Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.local_symbol))
    in
    Unpickler.create ~name:"Option_price"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~volatility:(fields_value (required float))
          ~underlying_price:(fields_value (required Price.val_type)))
      (fun contract volatility underlying_price ->
        { contract; volatility; underlying_price }))
end

module Implied_volatility = struct
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
    let contract_spec = Common_option_calc.Pickler.contract_spec in
    Pickler.create ~name:"Implied_volatility"
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
      Unpickler.Spec.(
        step (fun conv id symbol contract_type expiry strike option_right
          multiplier exchange listing_exchange currency local_symbol ->
            let contract = Raw_contract.create
              ?id ~expiry ~strike ~option_right ?multiplier ~exchange
              ?listing_exchange ~currency ?local_symbol ~contract_type
              symbol
            in
            conv contract
        )
        ++ value (optional Raw_contract.Id.val_type)
          ~name:(field_name Raw_contract.Fields.contract_id)
        ++ value (required Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.symbol)
        ++ value (required string)
          ~name:(field_name Raw_contract.Fields.contract_type)
        ++ value (required date)
          ~name:(field_name Raw_contract.Fields.expiry)
        ++ value (required Price.val_type)
          ~name:(field_name Raw_contract.Fields.strike)
        ++ value (required Raw_contract.Option_right.val_type)
          ~name:(field_name Raw_contract.Fields.option_right)
        ++ value (optional string)
          ~name:(field_name Raw_contract.Fields.multiplier)
        ++ value (required Exchange.val_type)
          ~name:(field_name Raw_contract.Fields.exchange)
        ++ value (optional Exchange.val_type)
          ~name:(field_name Raw_contract.Fields.listing_exchange)
        ++ value (required Currency.val_type)
          ~name:(field_name Raw_contract.Fields.currency)
        ++ value (optional Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.local_symbol))
    in
    Unpickler.create ~name:"Implied_volatility"
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
   | Executions                                                            |
   +-----------------------------------------------------------------------+ *)

module Execution_reports = struct
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
      contract_type : string;
      exchange : Exchange.t;
      order_action : Order.Action.t;
    } with sexp, fields

  let create ~contract ~client_id ~account_code ~time ~order_action =
    let contract = Contract.to_raw contract in
    { client_id;
      account_code;
      time;
      symbol = Raw_contract.symbol contract;
      contract_type = Raw_contract.contract_type contract;
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
      ~contract_type:(use (=))
      ~exchange:(use (=))
      ~order_action:(use (=))

  let pickler =
    Pickler.create ~name:"Execution_data"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~client_id:(fields_value (required Client_id.val_type))
            ~account_code:(fields_value (required Account_code.val_type))
            ~time:(fields_value (required Time.val_type))
            ~symbol:(fields_value (required Symbol.val_type))
            ~contract_type:(fields_value (required string))
            ~exchange:(fields_value (required Exchange.val_type))
            ~order_action:(fields_value (required Raw_order.Action.val_type)))
          (fun t ->
            `Args
              $ t.client_id
              $ t.account_code
              $ t.time
              $ t.symbol
              $ t.contract_type
              $ t.exchange
              $ t.order_action))

  let unpickler = Only_in_test.of_thunk (fun () ->
    Unpickler.create ~name:"Execution_data"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~client_id:(fields_value (required Client_id.val_type))
          ~account_code:(fields_value (required Account_code.val_type))
          ~time:(fields_value (required Time.val_type))
          ~symbol:(fields_value (required Symbol.val_type))
          ~contract_type:(fields_value (required string))
          ~exchange:(fields_value (required Exchange.val_type))
          ~order_action:(fields_value (required Raw_order.Action.val_type)))
      (fun client_id account_code time symbol contract_type exchange order_action ->
        { client_id;
          account_code;
          time;
          symbol;
          contract_type;
          exchange;
          order_action;
        }))
end

(* +-----------------------------------------------------------------------+
   | Contract specs                                                        |
   +-----------------------------------------------------------------------+ *)

module Contract_specs = struct
  type t = Raw_contract.t with sexp

  let create ~contract = Contract.to_raw contract
  let ( = ) t1 t2 = Raw_contract.(=) t1 t2

  let pickler =
    let contract_spec =
      Pickler.Spec.(
        Raw_contract.Fields.fold
          ~init:(empty ())
          ~contract_id:(fields_value (optional Raw_contract.Id.val_type))
          ~symbol:(fields_value (required Symbol.val_type))
          ~contract_type:(fields_value (required string))
          ~expiry:(fields_value (optional date))
          ~strike:(fields_value (optional Price.val_type))
          ~option_right:(fields_value (optional Raw_contract.Option_right.val_type))
          ~multiplier:(fields_value (optional string))
          ~exchange:(fields_value (required Exchange.val_type))
          ~listing_exchange:(fields_value skipped)
          ~currency:(fields_value (required Currency.val_type))
          ~local_symbol:(fields_value (optional Symbol.val_type))
          ~include_expired:(fields_value (required bool))
          ~security_id_type:(fields_value (optional Raw_contract.Security_id.Type.val_type))
          ~security_id:(fields_value (optional Raw_contract.Security_id.val_type))
          ~combo_legs:(fields_value skipped))
      |> wrap_contract_spec
    in
    Pickler.create ~name:"Contract_specs" contract_spec

  let unpickler = Only_in_test.of_thunk (fun () ->
    let contract_spec =
      Unpickler.Spec.(
        step (fun conv id symbol contract_type expiry strike option_right
          multiplier exchange currency local_symbol include_expired
          security_id_type security_id ->
            let contract = Raw_contract.create
              ?id ~contract_type ?expiry ?strike ?option_right ?multiplier
              ~exchange ~currency ?local_symbol ~include_expired
              ?security_id_type ?security_id symbol
            in
            conv contract
        )
        ++ value (optional Raw_contract.Id.val_type)
          ~name:(field_name Raw_contract.Fields.contract_id)
        ++ value (required Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.symbol)
        ++ value (required string)
          ~name:(field_name Raw_contract.Fields.contract_type)
        ++ value (optional date)
          ~name:(field_name Raw_contract.Fields.expiry)
        ++ value (optional Price.val_type)
          ~name:(field_name Raw_contract.Fields.strike)
        ++ value (optional Raw_contract.Option_right.val_type)
          ~name:(field_name Raw_contract.Fields.option_right)
        ++ value (optional string)
          ~name:(field_name Raw_contract.Fields.multiplier)
        ++ value (required Exchange.val_type)
          ~name:(field_name Raw_contract.Fields.exchange)
        ++ value (required Currency.val_type)
          ~name:(field_name Raw_contract.Fields.currency)
        ++ value (optional Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.local_symbol)
        ++ value (required bool)
          ~name:(field_name Raw_contract.Fields.include_expired)
        ++ value (optional Raw_contract.Security_id.Type.val_type)
          ~name:(field_name Raw_contract.Fields.security_id_type)
        ++ value (optional Raw_contract.Security_id.val_type)
          ~name:(field_name Raw_contract.Fields.security_id))
    in
    Unpickler.create ~name:"Contract_specs" contract_spec Fn.id)
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
      Pickler.Spec.(
        Raw_contract.Fields.fold
          ~init:(empty ())
          ~contract_id:(fields_value skipped)
          ~symbol:(fields_value (required Symbol.val_type))
          ~contract_type:(fields_value (required string))
          ~expiry:(fields_value (optional date))
          ~strike:(fields_value (optional Price.val_type))
          ~option_right:(fields_value (optional Raw_contract.Option_right.val_type))
          ~multiplier:(fields_value (optional string))
          ~exchange:(fields_value (required Exchange.val_type))
          ~listing_exchange:(fields_value skipped)
          ~currency:(fields_value (required Currency.val_type))
          ~local_symbol:(fields_value (optional Symbol.val_type))
          ~include_expired:(fields_value skipped)
          ~security_id_type:(fields_value skipped)
          ~security_id:(fields_value skipped)
          ~combo_legs:(fields_value skipped))
      |> wrap_contract_spec
    in
    Pickler.create ~name:"Market_depth"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~num_rows:(fields_value (required int)))
          (fun { contract; num_rows } -> `Args $ contract $ num_rows ))

  let unpickler = Only_in_test.of_thunk (fun () ->
    let contract_spec =
      Unpickler.Spec.(
        step (fun conv symbol contract_type expiry strike option_right
          multiplier exchange currency local_symbol ->
            let contract = Raw_contract.create
              ~contract_type ?expiry ?strike ?option_right ?multiplier
              ~exchange ~currency ?local_symbol symbol
            in
            conv contract
        )
        ++ value (required Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.symbol)
        ++ value (required string)
          ~name:(field_name Raw_contract.Fields.contract_type)
        ++ value (optional date)
          ~name:(field_name Raw_contract.Fields.expiry)
        ++ value (optional Price.val_type)
          ~name:(field_name Raw_contract.Fields.strike)
        ++ value (optional Raw_contract.Option_right.val_type)
          ~name:(field_name Raw_contract.Fields.option_right)
        ++ value (optional string)
          ~name:(field_name Raw_contract.Fields.multiplier)
        ++ value (required Exchange.val_type)
          ~name:(field_name Raw_contract.Fields.exchange)
        ++ value (required Currency.val_type)
          ~name:(field_name Raw_contract.Fields.currency)
        ++ value (optional Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.local_symbol))
    in
    Unpickler.create ~name:"Market_depth"
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

module Historical_data = struct
  module Bar_size = struct
    type t =
    [ `One_sec | `Five_secs | `Fifteen_secs | `Thirty_secs
    | `One_min | `Two_mins | `Three_mins | `Five_mins
    | `Fifteen_mins | `Thirty_mins
    | `One_hour
    | `One_day
    ] with sexp

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

  module Duration = struct
    type t = [ `S of int | `D of int | `W of int | `M of int | `Y ] with sexp

    let tws_of_t = function
      | `S x -> sprintf "%d S" x
      | `D x -> sprintf "%d D" x
      | `W x -> sprintf "%d W" x
      | `M x -> sprintf "%d M" x
      | `Y   -> sprintf "1 Y"

    let t_of_tws s =
      let extract_int s ~time_unit =
        let pattern = "\\([1-9][0-9]*\\)  " in
        String.nset pattern 16 time_unit;
        if Str.string_match (Str.regexp pattern) s 0 then
          Int.of_string (Str.matched_group 1 s)
        else invalid_argf "Duration.t_of_tws: %S" s ()
      in
      match String.nget s (String.length s - 1) with
      | 'S' -> `S (extract_int s ~time_unit:'S')
      | 'D' -> `D (extract_int s ~time_unit:'D')
      | 'W' -> `W (extract_int s ~time_unit:'W')
      | 'M' -> `M (extract_int s ~time_unit:'M')
      | 'Y' -> `Y
      | _ -> invalid_argf "Duration.t_of_tws: %S" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  module Show = struct
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
      | s -> invalid_argf "Show.t_of_tws: %S" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t =
    { contract : Raw_contract.t;
      end_date_time : Time.t;
      bar_size : Bar_size.t;
      duration : Duration.t;
      use_rth : bool;
      show : Show.t;
      date_format : string;
    } with sexp, fields

  let create ~contract ~end_date_time ~bar_size ~duration ~use_rth ~show =
    { contract = Contract.to_raw contract;
      end_date_time;
      bar_size;
      duration;
      use_rth;
      show;
      date_format = "1";
    }

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~contract:(use Raw_contract.(=))
      ~end_date_time:(use Time.(=))
      ~bar_size:(use (=))
      ~duration:(use (=))
      ~use_rth:(use (=))
      ~show:(use (=))
      ~date_format:(use (=))

  let pickler =
    let contract_spec =
      Pickler.Spec.(
        Raw_contract.Fields.fold
          ~init:(empty ())
          ~contract_id:(fields_value skipped)
          ~symbol:(fields_value (required Symbol.val_type))
          ~contract_type:(fields_value (required string))
          ~expiry:(fields_value (optional date))
          ~strike:(fields_value (optional Price.val_type))
          ~option_right:(fields_value (optional Raw_contract.Option_right.val_type))
          ~multiplier:(fields_value (optional string))
          ~exchange:(fields_value (required Exchange.val_type))
          ~listing_exchange:(fields_value (optional Exchange.val_type))
          ~currency:(fields_value (required Currency.val_type))
          ~local_symbol:(fields_value (optional Symbol.val_type))
          ~include_expired:(fields_value (required bool))
          ~security_id_type:(fields_value skipped)
          ~security_id:(fields_value skipped)
          ~combo_legs:(fields_value skipped))
      |> wrap_contract_spec
    in
    Pickler.create ~name:"Historical_data"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~end_date_time:(fields_value (required time))
            ~bar_size:(fields_value (required Bar_size.val_type))
            ~duration:(fields_value (required Duration.val_type))
            ~use_rth:(fields_value (required bool))
            ~show:(fields_value (required Show.val_type))
            ~date_format:(fields_value (required string)))
          (fun t ->
            `Args
              $ t.contract
              $ t.end_date_time
              $ t.bar_size
              $ t.duration
              $ t.use_rth
              $ t.show
              $ t.date_format))

  let unpickler = Only_in_test.of_thunk (fun () ->
    let contract_spec =
      Unpickler.Spec.(
        step (fun conv symbol contract_type expiry strike option_right multiplier
          exchange listing_exchange currency local_symbol include_expired ->
            let contract =
              Raw_contract.create ~contract_type ?expiry ?strike ?option_right
                ?multiplier ~exchange ?listing_exchange ~currency ?local_symbol
                ~include_expired symbol
            in
            conv contract
        )
        ++ value (required Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.symbol)
        ++ value (required string)
          ~name:(field_name Raw_contract.Fields.contract_type)
        ++ value (optional date)
          ~name:(field_name Raw_contract.Fields.expiry)
        ++ value (optional Price.val_type)
          ~name:(field_name Raw_contract.Fields.strike)
        ++ value (optional Raw_contract.Option_right.val_type)
          ~name:(field_name Raw_contract.Fields.option_right)
        ++ value (optional string)
          ~name:(field_name Raw_contract.Fields.multiplier)
        ++ value (required Exchange.val_type)
          ~name:(field_name Raw_contract.Fields.exchange)
        ++ value (optional Exchange.val_type)
          ~name:(field_name Raw_contract.Fields.listing_exchange)
        ++ value (required Currency.val_type)
          ~name:(field_name Raw_contract.Fields.currency)
        ++ value (optional Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.local_symbol)
        ++ value (required bool)
          ~name:(field_name Raw_contract.Fields.include_expired))
    in
    Unpickler.create ~name:"Historical_data"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~end_date_time:(fields_value (required time))
          ~bar_size:(fields_value (required Bar_size.val_type))
          ~duration:(fields_value (required Duration.val_type))
          ~use_rth:(fields_value (required bool))
          ~show:(fields_value (required Show.val_type))
          ~date_format:(fields_value (required string)))
      (fun contract end_date_time bar_size duration use_rth show date_format ->
        { contract;
          end_date_time;
          bar_size;
          duration;
          use_rth;
          show;
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

  module Show = struct
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
      | s -> invalid_argf "Show.t_of_tws: %s" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t =
    { contract : Raw_contract.t;
      bar_size : Bar_size.t;
      show : Show.t;
      use_rth : bool;
    } with sexp, fields

  let create ~contract ~bar_size ~show ~use_rth =
    { contract = Contract.to_raw contract;
      bar_size;
      show;
      use_rth;
    }

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~contract:(use Raw_contract.(=))
      ~bar_size:(use (=))
      ~show:(use (=))
      ~use_rth:(use (=))

  let pickler =
    let contract_spec =
      Pickler.Spec.(
        Raw_contract.Fields.fold
          ~init:(empty ())
          ~contract_id:(fields_value skipped)
          ~symbol:(fields_value (required Symbol.val_type))
          ~contract_type:(fields_value (required string))
          ~expiry:(fields_value (optional date))
          ~strike:(fields_value (optional Price.val_type))
          ~option_right:(fields_value (optional Raw_contract.Option_right.val_type))
          ~multiplier:(fields_value (optional string))
          ~exchange:(fields_value (required Exchange.val_type))
          ~listing_exchange:(fields_value (optional Exchange.val_type))
          ~currency:(fields_value (required Currency.val_type))
          ~local_symbol:(fields_value (optional Symbol.val_type))
          ~include_expired:(fields_value skipped)
          ~security_id_type:(fields_value skipped)
          ~security_id:(fields_value skipped)
          ~combo_legs:(fields_value skipped))
      |> wrap_contract_spec
    in
    Pickler.create ~name:"Realtime_bars"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~bar_size:(fields_value (required Bar_size.val_type))
            ~show:(fields_value (required Show.val_type))
            ~use_rth:(fields_value (required bool)))
          (fun { contract; bar_size; show; use_rth } ->
            `Args $ contract $ bar_size $ show $ use_rth ))

  let unpickler = Only_in_test.of_thunk (fun () ->
    let contract_spec =
      Unpickler.Spec.(
        step (fun conv symbol contract_type expiry strike option_right
          multiplier exchange listing_exchange currency local_symbol ->
            let contract =
              Raw_contract.create ~contract_type ?expiry ?strike ?option_right
                ?multiplier ~exchange ?listing_exchange ~currency ?local_symbol
                symbol
            in
            conv contract
        )
        ++ value (required Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.symbol)
        ++ value (required string)
          ~name:(field_name Raw_contract.Fields.contract_type)
        ++ value (optional date)
          ~name:(field_name Raw_contract.Fields.expiry)
        ++ value (optional Price.val_type)
          ~name:(field_name Raw_contract.Fields.strike)
        ++ value (optional Raw_contract.Option_right.val_type)
          ~name:(field_name Raw_contract.Fields.option_right)
        ++ value (optional string)
          ~name:(field_name Raw_contract.Fields.multiplier)
        ++ value (required Exchange.val_type)
          ~name:(field_name Raw_contract.Fields.exchange)
        ++ value (optional Exchange.val_type)
          ~name:(field_name Raw_contract.Fields.listing_exchange)
        ++ value (required Currency.val_type)
          ~name:(field_name Raw_contract.Fields.currency)
        ++ value (optional Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.local_symbol))
    in
    Unpickler.create ~name:"Realtime_bars"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~bar_size:(fields_value (required Bar_size.val_type))
          ~show:(fields_value (required Show.val_type))
          ~use_rth:(fields_value (required bool)))
      (fun contract bar_size show use_rth ->
        { contract; bar_size; show; use_rth }))
end
