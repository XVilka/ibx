(* File: raw_contract.ml

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
  let val_type = Val_type.create to_string of_string
end

module Security_type = struct
  type t = [ `Stock | `Futures | `Option | `Forex ] with sexp

  let tws_of_t = function
    | `Stock   -> "STK"
    | `Futures -> "FUT"
    | `Option  -> "OPT"
    | `Forex   -> "CASH"

  let t_of_tws = function
  	| "STK"  -> `Stock
	| "FUT"  -> `Futures
	| "OPT"  -> `Option
    | "CASH" -> `Forex
    | s -> invalid_argf "Type.t_of_tws: %S" s ()

  let val_type = Val_type.create tws_of_t t_of_tws
end

module Security_id = struct
  include String
  module Type = struct
    type t = [ `ISIN | `CUSIP | `SEDOL | `RIC ] with sexp

    let tws_of_t = function
      | `ISIN  -> "ISIN"
      | `CUSIP -> "CUSIP"
      | `SEDOL -> "SEDOL"
      | `RIC   -> "RIC"

    let t_of_tws = function
      | "ISIN"  -> `ISIN
      | "CUSIP" -> `CUSIP
      | "SEDOL" -> `SEDOL
      | "RIC"   -> `RIC
      | s -> invalid_argf "Security_id.Type.t_of_tws: %S" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  let tws_of_t = to_string
  let t_of_tws = of_string
  let val_type = Val_type.create tws_of_t t_of_tws
end

module Option_right = struct
  type t = [ `Call | `Put ] with sexp

  let tws_of_t = function
    | `Call -> "C"
    | `Put  -> "P"

  let t_of_tws = function
    | "C" -> `Call
    | "P" -> `Put
    | s -> invalid_argf "Option_right.t_of_tws: %S" s ()

  let val_type = Val_type.create tws_of_t t_of_tws
end

type t =
  { contract_id : Id.t option;
    symbol : Symbol.t;
    security_type : string;
    expiry : Date.t option;
    strike : Price.t option;
    option_right : Option_right.t option;
    multiplier : string option;
    exchange : Exchange.t;
    listing_exchange : Exchange.t option;
    currency : Currency.t;
    local_symbol : Symbol.t option;
    include_expired : bool;
    security_id_type : Security_id.Type.t option;
    security_id : Security_id.t option;
    combo_legs : int;
  }
with sexp, fields

let create ?id ?expiry ?strike ?option_right ?multiplier ?listing_exchange
    ?local_symbol ?security_id_type ?security_id ?(include_expired=false)
    ?(exchange=`SMART) ~currency ~security_type symbol =
  { contract_id = id;
    symbol;
    security_type;
    expiry;
    strike;
    option_right;
    multiplier;
    exchange;
    listing_exchange;
    currency;
    local_symbol;
    include_expired;
    security_id_type;
    security_id;
    combo_legs = 0;
  }

let ( = ) t1 t2 : bool =
  let use op = fun field ->
    op (Field.get field t1) (Field.get field t2)
  in
  Fields.for_all
    ~contract_id:(use (=))
    ~symbol:(use (=))
    ~security_type:(use (=))
    ~expiry:(use (=))
    ~strike:(use (Option.equal Price.(=.)))
    ~option_right:(use (=))
    ~multiplier:(use (=))
    ~exchange:(use (=))
    ~listing_exchange:(use (=))
    ~currency:(use (=))
    ~local_symbol:(use (=))
    ~include_expired:(use (=))
    ~security_id_type:(use (=))
    ~security_id:(use (=))
    ~combo_legs:(use (=))


module Pickler_specs = struct

  let wrap_contract_spec contract_spec =
    Pickler.Spec.(
      wrap contract_spec
        (fun t ->
          `Args
            $ t.contract_id
            $ t.symbol
            $ t.security_type
            $ t.expiry
            $ t.strike
            $ t.option_right
            $ t.multiplier
            $ t.exchange
            $ t.listing_exchange
            $ t.currency
            $ t.local_symbol
            $ t.include_expired
            $ t.security_id_type
            $ t.security_id
            $ t.combo_legs))

  let market_data_query () =
    Pickler.Spec.(
      Fields.fold
        ~init:(empty ())
        ~contract_id:(fields_value (optional Id.val_type))
        ~symbol:(fields_value (required Symbol.val_type))
        ~security_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type))
        ~option_right:(fields_value (optional Option_right.val_type))
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

  let common_option_calc () =
    Pickler.Spec.(
      Fields.fold
        ~init:(empty ())
        ~contract_id:(fields_value (optional Id.val_type))
        ~symbol:(fields_value (required Symbol.val_type))
        ~security_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type))
        ~option_right:(fields_value (optional Option_right.val_type))
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

  let contract_details_query () =
    Pickler.Spec.(
      Fields.fold
        ~init:(empty ())
        ~contract_id:(fields_value (optional Id.val_type))
        ~symbol:(fields_value (required Symbol.val_type))
        ~security_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type))
        ~option_right:(fields_value (optional Option_right.val_type))
        ~multiplier:(fields_value (optional string))
        ~exchange:(fields_value (required Exchange.val_type))
        ~listing_exchange:(fields_value skipped)
        ~currency:(fields_value (required Currency.val_type))
        ~local_symbol:(fields_value (optional Symbol.val_type))
        ~include_expired:(fields_value (required bool))
        ~security_id_type:(fields_value (optional Security_id.Type.val_type))
        ~security_id:(fields_value (optional Security_id.val_type))
        ~combo_legs:(fields_value skipped))
    |> wrap_contract_spec

  let market_depth_query () =
    Pickler.Spec.(
      Fields.fold
        ~init:(empty ())
        ~contract_id:(fields_value skipped)
        ~symbol:(fields_value (required Symbol.val_type))
        ~security_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type))
        ~option_right:(fields_value (optional Option_right.val_type))
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

  let historical_data_query () =
    Pickler.Spec.(
      Fields.fold
        ~init:(empty ())
        ~contract_id:(fields_value skipped)
        ~symbol:(fields_value (required Symbol.val_type))
        ~security_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type))
        ~option_right:(fields_value (optional Option_right.val_type))
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

  let realtime_bars_query () =
    Pickler.Spec.(
      Fields.fold
        ~init:(empty ())
        ~contract_id:(fields_value skipped)
        ~symbol:(fields_value (required Symbol.val_type))
        ~security_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type))
        ~option_right:(fields_value (optional Option_right.val_type))
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

  let portfolio_update_response () =
    Pickler.Spec.(
      Fields.fold
        ~init:(empty ())
        ~contract_id:(fields_value (optional Id.val_type))
        ~symbol:(fields_value (required Symbol.val_type))
        ~security_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type ~default_on_none:"0"))
        ~option_right:(fields_value (optional Option_right.val_type
                                       ~default_on_none:"0"))
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

  let execution_report_response () =
    Pickler.Spec.(
      Fields.fold
        ~init:(empty ())
        ~contract_id:(fields_value (optional Id.val_type))
        ~symbol:(fields_value (required Symbol.val_type))
        ~security_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type ~default_on_none:"0.0"))
        ~option_right:(fields_value (optional Option_right.val_type))
        ~multiplier:(fields_value skipped)
        ~exchange:(fields_value (required Exchange.val_type))
        ~listing_exchange:(fields_value skipped)
        ~currency:(fields_value (required Currency.val_type))
        ~local_symbol:(fields_value (optional Symbol.val_type))
        ~include_expired:(fields_value skipped)
        ~security_id_type:(fields_value skipped)
        ~security_id:(fields_value skipped)
        ~combo_legs:(fields_value skipped))
    |> wrap_contract_spec
end

module Unpickler_specs = struct

  let field_name field = Fieldslib.Field.name field

  let market_data_query () =
    Unpickler.Spec.(
      step (fun conv id symbol security_type expiry strike option_right
        multiplier exchange listing_exchange currency local_symbol
        _combo_legs ->
          let contract = create
            ?id ~security_type ?expiry ?strike ?option_right ?multiplier
            ~exchange ?listing_exchange ~currency ?local_symbol symbol
          in
          conv contract
      )
      ++ value (optional Id.val_type)
        ~name:(field_name Fields.contract_id)
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.security_type)
      ++ value (optional date)
        ~name:(field_name Fields.expiry)
      ++ value (optional Price.val_type)
        ~name:(field_name Fields.strike)
      ++ value (optional Option_right.val_type)
        ~name:(field_name Fields.option_right)
      ++ value (optional string)
        ~name:(field_name Fields.multiplier)
      ++ value (required Exchange.val_type)
        ~name:(field_name Fields.exchange)
      ++ value (optional Exchange.val_type)
        ~name:(field_name Fields.listing_exchange)
      ++ value (required Currency.val_type)
        ~name:(field_name Fields.currency)
      ++ value (optional Symbol.val_type)
        ~name:(field_name Fields.local_symbol)
      ++ value (required int)
        ~name:(field_name Fields.combo_legs))

  let option_price_query () =
    Unpickler.Spec.(
      step (fun conv id symbol security_type expiry strike option_right
        multiplier exchange listing_exchange currency local_symbol ->
          let contract = create
            ?id ~expiry ~strike ~option_right ?multiplier ~exchange
            ?listing_exchange ~currency ?local_symbol ~security_type
            symbol
          in
          conv contract
      )
      ++ value (optional Id.val_type)
        ~name:(field_name Fields.contract_id)
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.security_type)
      ++ value (required date)
        ~name:(field_name Fields.expiry)
      ++ value (required Price.val_type)
        ~name:(field_name Fields.strike)
      ++ value (required Option_right.val_type)
        ~name:(field_name Fields.option_right)
      ++ value (optional string)
        ~name:(field_name Fields.multiplier)
      ++ value (required Exchange.val_type)
        ~name:(field_name Fields.exchange)
      ++ value (optional Exchange.val_type)
        ~name:(field_name Fields.listing_exchange)
      ++ value (required Currency.val_type)
        ~name:(field_name Fields.currency)
      ++ value (optional Symbol.val_type)
        ~name:(field_name Fields.local_symbol))

  let implied_volatility_query () =
    Unpickler.Spec.(
      step (fun conv id symbol security_type expiry strike option_right
        multiplier exchange listing_exchange currency local_symbol ->
          let contract = create
            ?id ~expiry ~strike ~option_right ?multiplier ~exchange
            ?listing_exchange ~currency ?local_symbol ~security_type
            symbol
          in
          conv contract
      )
      ++ value (optional Id.val_type)
        ~name:(field_name Fields.contract_id)
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.security_type)
      ++ value (required date)
        ~name:(field_name Fields.expiry)
      ++ value (required Price.val_type)
        ~name:(field_name Fields.strike)
      ++ value (required Option_right.val_type)
        ~name:(field_name Fields.option_right)
      ++ value (optional string)
        ~name:(field_name Fields.multiplier)
      ++ value (required Exchange.val_type)
        ~name:(field_name Fields.exchange)
      ++ value (optional Exchange.val_type)
        ~name:(field_name Fields.listing_exchange)
      ++ value (required Currency.val_type)
        ~name:(field_name Fields.currency)
      ++ value (optional Symbol.val_type)
        ~name:(field_name Fields.local_symbol))

  let contract_details_query () =
    Unpickler.Spec.(
      step (fun conv id symbol security_type expiry strike option_right
        multiplier exchange currency local_symbol include_expired
        security_id_type security_id ->
          let contract = create
            ?id ~security_type ?expiry ?strike ?option_right ?multiplier
            ~exchange ~currency ?local_symbol ~include_expired
            ?security_id_type ?security_id symbol
          in
          conv contract
      )
      ++ value (optional Id.val_type)
        ~name:(field_name Fields.contract_id)
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.security_type)
      ++ value (optional date)
        ~name:(field_name Fields.expiry)
      ++ value (optional Price.val_type)
        ~name:(field_name Fields.strike)
      ++ value (optional Option_right.val_type)
        ~name:(field_name Fields.option_right)
      ++ value (optional string)
        ~name:(field_name Fields.multiplier)
      ++ value (required Exchange.val_type)
        ~name:(field_name Fields.exchange)
      ++ value (required Currency.val_type)
        ~name:(field_name Fields.currency)
      ++ value (optional Symbol.val_type)
        ~name:(field_name Fields.local_symbol)
      ++ value (required bool)
        ~name:(field_name Fields.include_expired)
      ++ value (optional Security_id.Type.val_type)
        ~name:(field_name Fields.security_id_type)
      ++ value (optional Security_id.val_type)
        ~name:(field_name Fields.security_id))

  let market_depth_query () =
    Unpickler.Spec.(
      step (fun conv symbol security_type expiry strike option_right
        multiplier exchange currency local_symbol ->
          let contract = create
            ~security_type ?expiry ?strike ?option_right ?multiplier
            ~exchange ~currency ?local_symbol symbol
          in
          conv contract
      )
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.security_type)
      ++ value (optional date)
        ~name:(field_name Fields.expiry)
      ++ value (optional Price.val_type)
        ~name:(field_name Fields.strike)
      ++ value (optional Option_right.val_type)
        ~name:(field_name Fields.option_right)
      ++ value (optional string)
        ~name:(field_name Fields.multiplier)
      ++ value (required Exchange.val_type)
        ~name:(field_name Fields.exchange)
      ++ value (required Currency.val_type)
        ~name:(field_name Fields.currency)
      ++ value (optional Symbol.val_type)
        ~name:(field_name Fields.local_symbol))

  let historical_data_query () =
    Unpickler.Spec.(
      step (fun conv symbol security_type expiry strike option_right multiplier
        exchange listing_exchange currency local_symbol include_expired ->
          let contract = create
            ~security_type ?expiry ?strike ?option_right ?multiplier ~exchange
            ?listing_exchange ~currency ?local_symbol ~include_expired symbol
          in
          conv contract
      )
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.security_type)
      ++ value (optional date)
        ~name:(field_name Fields.expiry)
      ++ value (optional Price.val_type)
        ~name:(field_name Fields.strike)
      ++ value (optional Option_right.val_type)
        ~name:(field_name Fields.option_right)
      ++ value (optional string)
        ~name:(field_name Fields.multiplier)
      ++ value (required Exchange.val_type)
        ~name:(field_name Fields.exchange)
      ++ value (optional Exchange.val_type)
        ~name:(field_name Fields.listing_exchange)
      ++ value (required Currency.val_type)
        ~name:(field_name Fields.currency)
      ++ value (optional Symbol.val_type)
        ~name:(field_name Fields.local_symbol)
      ++ value (required bool)
        ~name:(field_name Fields.include_expired))

  let realtime_bars_query () =
    Unpickler.Spec.(
      step (fun conv symbol security_type expiry strike option_right
        multiplier exchange listing_exchange currency local_symbol ->
          let contract = create
            ~security_type ?expiry ?strike ?option_right ?multiplier ~exchange
            ?listing_exchange ~currency ?local_symbol symbol
          in
          conv contract
      )
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.security_type)
      ++ value (optional date)
        ~name:(field_name Fields.expiry)
      ++ value (optional Price.val_type)
        ~name:(field_name Fields.strike)
      ++ value (optional Option_right.val_type)
        ~name:(field_name Fields.option_right)
      ++ value (optional string)
        ~name:(field_name Fields.multiplier)
      ++ value (required Exchange.val_type)
        ~name:(field_name Fields.exchange)
      ++ value (optional Exchange.val_type)
        ~name:(field_name Fields.listing_exchange)
      ++ value (required Currency.val_type)
        ~name:(field_name Fields.currency)
      ++ value (optional Symbol.val_type)
        ~name:(field_name Fields.local_symbol))

  let portfolio_update_response () =
    Unpickler.Spec.(
      step (fun conv id symbol security_type expiry strike option_right
        multiplier exchange currency local_symbol ->
          let contract = create
            ?id ~security_type ?expiry ?strike ?option_right ?multiplier
            ~exchange ~currency ?local_symbol symbol
          in
          conv contract
      )
      ++ value (optional Id.val_type)
        ~name:(field_name Fields.contract_id)
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.security_type)
      ++ value (optional date)
        ~name:(field_name Fields.expiry)
      ++ value (optional Price.val_type ~none_on_default:"0")
        ~name:(field_name Fields.strike)
      ++ value (optional Option_right.val_type ~none_on_default:"0")
        ~name:(field_name Fields.option_right)
      ++ value (optional string)
        ~name:(field_name Fields.multiplier)
      ++ value (required Exchange.val_type)
        ~name:(field_name Fields.exchange)
      ++ value (required Currency.val_type)
        ~name:(field_name Fields.currency)
      ++ value (optional Symbol.val_type)
        ~name:(field_name Fields.local_symbol))

  let execution_report_response () =
    Unpickler.Spec.(
      step (fun conv id symbol security_type expiry strike option_right
        exchange currency local_symbol ->
          let contract = create
            ?id ~security_type ?expiry ?strike ?option_right ~exchange
            ~currency ?local_symbol symbol
          in
          conv contract
      )
      ++ value (optional Id.val_type)
        ~name:(field_name Fields.contract_id)
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.security_type)
      ++ value (optional date)
        ~name:(field_name Fields.expiry)
      ++ value (optional Price.val_type ~none_on_default:"0.0")
        ~name:(field_name Fields.strike)
      ++ value (optional Option_right.val_type)
        ~name:(field_name Fields.option_right)
      ++ value (required Exchange.val_type)
        ~name:(field_name Fields.exchange)
      ++ value (required Currency.val_type)
        ~name:(field_name Fields.currency)
      ++ value (optional Symbol.val_type)
        ~name:(field_name Fields.local_symbol))
end
