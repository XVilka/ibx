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

type t =
  { con_id : Contract_id.t option;
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
    include_expired : bool;
    sec_id_type : Security_id.Type.t option;
    sec_id : Security_id.Id.t option;
    combo_legs : int;
  }
[@@deriving sexp, fields]

let create ?con_id ?expiry ?strike ?right ?multiplier ?prim_exch
    ?local_symbol ?sec_id_type ?sec_id ?(include_expired=false)
    ?(exchange=`SMART) ~currency ~sec_type symbol =
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
    include_expired;
    sec_id_type;
    sec_id;
    combo_legs = 0;
  }

let ( = ) t1 t2 : bool =
  let use op = fun field ->
    op (Field.get field t1) (Field.get field t2)
  in
  Fields.for_all
    ~con_id:(use (=))
    ~symbol:(use (=))
    ~sec_type:(use (=))
    ~expiry:(use (=))
    ~strike:(use (Option.equal Price.(=.)))
    ~right:(use (=))
    ~multiplier:(use (=))
    ~exchange:(use (=))
    ~prim_exch:(use (=))
    ~currency:(use (=))
    ~local_symbol:(use (=))
    ~include_expired:(use (=))
    ~sec_id_type:(use (=))
    ~sec_id:(use (=))
    ~combo_legs:(use (=))


module Pickler_specs = struct

  let lift_contract_spec contract_spec =
    Pickler.Spec.(
      lift contract_spec
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
           $ t.include_expired
           $ t.sec_id_type
           $ t.sec_id
           $ t.combo_legs))

  let market_data_query () =
    Pickler.Spec.(
      Fields.fold
        ~init:(empty ())
        ~con_id:(fields_value (optional Contract_id.val_type))
        ~symbol:(fields_value (required Symbol.val_type))
        ~sec_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type))
        ~right:(fields_value (optional Option_right.val_type))
        ~multiplier:(fields_value (optional int))
        ~exchange:(fields_value (required Exchange.val_type))
        ~prim_exch:(fields_value (optional Exchange.val_type))
        ~currency:(fields_value (required Currency.val_type))
        ~local_symbol:(fields_value (optional Symbol.val_type))
        ~include_expired:(fields_value skipped)
        ~sec_id_type:(fields_value skipped)
        ~sec_id:(fields_value skipped)
        ~combo_legs:(fields_value (required int)))
    |> lift_contract_spec

  let common_option_calc () =
    Pickler.Spec.(
      Fields.fold
        ~init:(empty ())
        ~con_id:(fields_value (optional Contract_id.val_type))
        ~symbol:(fields_value (required Symbol.val_type))
        ~sec_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type))
        ~right:(fields_value (optional Option_right.val_type))
        ~multiplier:(fields_value (optional int))
        ~exchange:(fields_value (required Exchange.val_type))
        ~prim_exch:(fields_value (optional Exchange.val_type))
        ~currency:(fields_value (required Currency.val_type))
        ~local_symbol:(fields_value (optional Symbol.val_type))
        ~include_expired:(fields_value skipped)
        ~sec_id_type:(fields_value skipped)
        ~sec_id:(fields_value skipped)
        ~combo_legs:(fields_value skipped))
    |> lift_contract_spec

  let contract_details_query () =
    Pickler.Spec.(
      Fields.fold
        ~init:(empty ())
        ~con_id:(fields_value (optional Contract_id.val_type))
        ~symbol:(fields_value (required Symbol.val_type))
        ~sec_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type))
        ~right:(fields_value (optional Option_right.val_type))
        ~multiplier:(fields_value (optional int))
        ~exchange:(fields_value (required Exchange.val_type))
        ~prim_exch:(fields_value skipped)
        ~currency:(fields_value (required Currency.val_type))
        ~local_symbol:(fields_value (optional Symbol.val_type))
        ~include_expired:(fields_value (required bool))
        ~sec_id_type:(fields_value (optional Security_id.Type.val_type))
        ~sec_id:(fields_value (optional Security_id.Id.val_type))
        ~combo_legs:(fields_value skipped))
    |> lift_contract_spec

  let market_depth_query () =
    Pickler.Spec.(
      Fields.fold
        ~init:(empty ())
        ~con_id:(fields_value skipped)
        ~symbol:(fields_value (required Symbol.val_type))
        ~sec_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type))
        ~right:(fields_value (optional Option_right.val_type))
        ~multiplier:(fields_value (optional int))
        ~exchange:(fields_value (required Exchange.val_type))
        ~prim_exch:(fields_value skipped)
        ~currency:(fields_value (required Currency.val_type))
        ~local_symbol:(fields_value (optional Symbol.val_type))
        ~include_expired:(fields_value skipped)
        ~sec_id_type:(fields_value skipped)
        ~sec_id:(fields_value skipped)
        ~combo_legs:(fields_value skipped))
    |> lift_contract_spec

  let history_query () =
    Pickler.Spec.(
      Fields.fold
        ~init:(empty ())
        ~con_id:(fields_value skipped)
        ~symbol:(fields_value (required Symbol.val_type))
        ~sec_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type))
        ~right:(fields_value (optional Option_right.val_type))
        ~multiplier:(fields_value (optional int))
        ~exchange:(fields_value (required Exchange.val_type))
        ~prim_exch:(fields_value (optional Exchange.val_type))
        ~currency:(fields_value (required Currency.val_type))
        ~local_symbol:(fields_value (optional Symbol.val_type))
        ~include_expired:(fields_value (required bool))
        ~sec_id_type:(fields_value skipped)
        ~sec_id:(fields_value skipped)
        ~combo_legs:(fields_value skipped))
    |> lift_contract_spec

  let realtime_bars_query () =
    Pickler.Spec.(
      Fields.fold
        ~init:(empty ())
        ~con_id:(fields_value skipped)
        ~symbol:(fields_value (required Symbol.val_type))
        ~sec_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type))
        ~right:(fields_value (optional Option_right.val_type))
        ~multiplier:(fields_value (optional int))
        ~exchange:(fields_value (required Exchange.val_type))
        ~prim_exch:(fields_value (optional Exchange.val_type))
        ~currency:(fields_value (required Currency.val_type))
        ~local_symbol:(fields_value (optional Symbol.val_type))
        ~include_expired:(fields_value skipped)
        ~sec_id_type:(fields_value skipped)
        ~sec_id:(fields_value skipped)
        ~combo_legs:(fields_value skipped))
    |> lift_contract_spec

  let position_response () =
    Pickler.Spec.(
      Fields.fold
        ~init:(empty ())
        ~con_id:(fields_value (optional Contract_id.val_type))
        ~symbol:(fields_value (required Symbol.val_type))
        ~sec_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type ~default_on_none:"0"))
        ~right:(fields_value (optional Option_right.val_type ~default_on_none:"0"))
        ~multiplier:(fields_value (optional int))
        ~exchange:(fields_value (required Exchange.val_type))
        ~prim_exch:(fields_value skipped)
        ~currency:(fields_value (required Currency.val_type))
        ~local_symbol:(fields_value (optional Symbol.val_type))
        ~include_expired:(fields_value skipped)
        ~sec_id_type:(fields_value skipped)
        ~sec_id:(fields_value skipped)
        ~combo_legs:(fields_value skipped))
    |> lift_contract_spec

  let execution_response () =
    Pickler.Spec.(
      Fields.fold
        ~init:(empty ())
        ~con_id:(fields_value (optional Contract_id.val_type))
        ~symbol:(fields_value (required Symbol.val_type))
        ~sec_type:(fields_value (required string))
        ~expiry:(fields_value (optional date))
        ~strike:(fields_value (optional Price.val_type ~default_on_none:"0.0"))
        ~right:(fields_value (optional Option_right.val_type))
        ~multiplier:(fields_value skipped)
        ~exchange:(fields_value (required Exchange.val_type))
        ~prim_exch:(fields_value skipped)
        ~currency:(fields_value (required Currency.val_type))
        ~local_symbol:(fields_value (optional Symbol.val_type))
        ~include_expired:(fields_value skipped)
        ~sec_id_type:(fields_value skipped)
        ~sec_id:(fields_value skipped)
        ~combo_legs:(fields_value skipped))
    |> lift_contract_spec
end

module Unpickler_specs = struct

  let field_name field = Fieldslib.Field.name field

  let market_data_query () =
    Unpickler.Spec.(
      step (fun conv con_id symbol sec_type expiry strike right
             multiplier exchange prim_exch currency local_symbol
             _combo_legs ->
             let contract = create
                 ?con_id ~sec_type ?expiry ?strike ?right ?multiplier
                 ~exchange ?prim_exch ~currency ?local_symbol symbol
             in
             conv contract
           )
      ++ value (optional Contract_id.val_type)
        ~name:(field_name Fields.con_id)
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.sec_type)
      ++ value (optional date)
        ~name:(field_name Fields.expiry)
      ++ value (optional Price.val_type)
        ~name:(field_name Fields.strike)
      ++ value (optional Option_right.val_type)
        ~name:(field_name Fields.right)
      ++ value (optional int)
        ~name:(field_name Fields.multiplier)
      ++ value (required Exchange.val_type)
        ~name:(field_name Fields.exchange)
      ++ value (optional Exchange.val_type)
        ~name:(field_name Fields.prim_exch)
      ++ value (required Currency.val_type)
        ~name:(field_name Fields.currency)
      ++ value (optional Symbol.val_type)
        ~name:(field_name Fields.local_symbol)
      ++ value (required int)
        ~name:(field_name Fields.combo_legs))

  let option_price_query () =
    Unpickler.Spec.(
      step (fun conv con_id symbol sec_type expiry strike right
             multiplier exchange prim_exch currency local_symbol ->
             let contract = create
                 ?con_id ~expiry ~strike ~right ?multiplier ~exchange
                 ?prim_exch ~currency ?local_symbol ~sec_type
                 symbol
             in
             conv contract
           )
      ++ value (optional Contract_id.val_type)
        ~name:(field_name Fields.con_id)
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.sec_type)
      ++ value (required date)
        ~name:(field_name Fields.expiry)
      ++ value (required Price.val_type)
        ~name:(field_name Fields.strike)
      ++ value (required Option_right.val_type)
        ~name:(field_name Fields.right)
      ++ value (optional int)
        ~name:(field_name Fields.multiplier)
      ++ value (required Exchange.val_type)
        ~name:(field_name Fields.exchange)
      ++ value (optional Exchange.val_type)
        ~name:(field_name Fields.prim_exch)
      ++ value (required Currency.val_type)
        ~name:(field_name Fields.currency)
      ++ value (optional Symbol.val_type)
        ~name:(field_name Fields.local_symbol))

  let implied_volatility_query () =
    Unpickler.Spec.(
      step (fun conv con_id symbol sec_type expiry strike right
             multiplier exchange prim_exch currency local_symbol ->
             let contract = create
                 ?con_id ~expiry ~strike ~right ?multiplier ~exchange
                 ?prim_exch ~currency ?local_symbol ~sec_type
                 symbol
             in
             conv contract
           )
      ++ value (optional Contract_id.val_type)
        ~name:(field_name Fields.con_id)
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.sec_type)
      ++ value (required date)
        ~name:(field_name Fields.expiry)
      ++ value (required Price.val_type)
        ~name:(field_name Fields.strike)
      ++ value (required Option_right.val_type)
        ~name:(field_name Fields.right)
      ++ value (optional int)
        ~name:(field_name Fields.multiplier)
      ++ value (required Exchange.val_type)
        ~name:(field_name Fields.exchange)
      ++ value (optional Exchange.val_type)
        ~name:(field_name Fields.prim_exch)
      ++ value (required Currency.val_type)
        ~name:(field_name Fields.currency)
      ++ value (optional Symbol.val_type)
        ~name:(field_name Fields.local_symbol))

  let contract_details_query () =
    Unpickler.Spec.(
      step (fun conv con_id symbol sec_type expiry strike right
             multiplier exchange currency local_symbol include_expired
             sec_id_type sec_id ->
             let contract = create
                 ?con_id ~sec_type ?expiry ?strike ?right ?multiplier
                 ~exchange ~currency ?local_symbol ~include_expired
                 ?sec_id_type ?sec_id symbol
             in
             conv contract
           )
      ++ value (optional Contract_id.val_type)
        ~name:(field_name Fields.con_id)
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.sec_type)
      ++ value (optional date)
        ~name:(field_name Fields.expiry)
      ++ value (optional Price.val_type)
        ~name:(field_name Fields.strike)
      ++ value (optional Option_right.val_type)
        ~name:(field_name Fields.right)
      ++ value (optional int)
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
        ~name:(field_name Fields.sec_id_type)
      ++ value (optional Security_id.Id.val_type)
        ~name:(field_name Fields.sec_id))

  let market_depth_query () =
    Unpickler.Spec.(
      step (fun conv symbol sec_type expiry strike right
             multiplier exchange currency local_symbol ->
             let contract = create
                 ~sec_type ?expiry ?strike ?right ?multiplier
                 ~exchange ~currency ?local_symbol symbol
             in
             conv contract
           )
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.sec_type)
      ++ value (optional date)
        ~name:(field_name Fields.expiry)
      ++ value (optional Price.val_type)
        ~name:(field_name Fields.strike)
      ++ value (optional Option_right.val_type)
        ~name:(field_name Fields.right)
      ++ value (optional int)
        ~name:(field_name Fields.multiplier)
      ++ value (required Exchange.val_type)
        ~name:(field_name Fields.exchange)
      ++ value (required Currency.val_type)
        ~name:(field_name Fields.currency)
      ++ value (optional Symbol.val_type)
        ~name:(field_name Fields.local_symbol))

  let history_query () =
    Unpickler.Spec.(
      step (fun conv symbol sec_type expiry strike right multiplier
             exchange prim_exch currency local_symbol include_expired ->
             let contract = create
                 ~sec_type ?expiry ?strike ?right ?multiplier ~exchange
                 ?prim_exch ~currency ?local_symbol ~include_expired symbol
             in
             conv contract
           )
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.sec_type)
      ++ value (optional date)
        ~name:(field_name Fields.expiry)
      ++ value (optional Price.val_type)
        ~name:(field_name Fields.strike)
      ++ value (optional Option_right.val_type)
        ~name:(field_name Fields.right)
      ++ value (optional int)
        ~name:(field_name Fields.multiplier)
      ++ value (required Exchange.val_type)
        ~name:(field_name Fields.exchange)
      ++ value (optional Exchange.val_type)
        ~name:(field_name Fields.prim_exch)
      ++ value (required Currency.val_type)
        ~name:(field_name Fields.currency)
      ++ value (optional Symbol.val_type)
        ~name:(field_name Fields.local_symbol)
      ++ value (required bool)
        ~name:(field_name Fields.include_expired))

  let realtime_bars_query () =
    Unpickler.Spec.(
      step (fun conv symbol sec_type expiry strike right
             multiplier exchange prim_exch currency local_symbol ->
             let contract = create
                 ~sec_type ?expiry ?strike ?right ?multiplier ~exchange
                 ?prim_exch ~currency ?local_symbol symbol
             in
             conv contract
           )
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.sec_type)
      ++ value (optional date)
        ~name:(field_name Fields.expiry)
      ++ value (optional Price.val_type)
        ~name:(field_name Fields.strike)
      ++ value (optional Option_right.val_type)
        ~name:(field_name Fields.right)
      ++ value (optional int)
        ~name:(field_name Fields.multiplier)
      ++ value (required Exchange.val_type)
        ~name:(field_name Fields.exchange)
      ++ value (optional Exchange.val_type)
        ~name:(field_name Fields.prim_exch)
      ++ value (required Currency.val_type)
        ~name:(field_name Fields.currency)
      ++ value (optional Symbol.val_type)
        ~name:(field_name Fields.local_symbol))

  let position_response () =
    Unpickler.Spec.(
      step (fun conv con_id symbol sec_type expiry strike right
             multiplier exchange currency local_symbol ->
             let contract = create
                 ?con_id ~sec_type ?expiry ?strike ?right ?multiplier
                 ~exchange ~currency ?local_symbol symbol
             in
             conv contract
           )
      ++ value (optional Contract_id.val_type)
        ~name:(field_name Fields.con_id)
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.sec_type)
      ++ value (optional date)
        ~name:(field_name Fields.expiry)
      ++ value (optional Price.val_type ~none_on_default:"0")
        ~name:(field_name Fields.strike)
      ++ value (optional Option_right.val_type ~none_on_default:"0")
        ~name:(field_name Fields.right)
      ++ value (optional int)
        ~name:(field_name Fields.multiplier)
      ++ value (required Exchange.val_type)
        ~name:(field_name Fields.exchange)
      ++ value (required Currency.val_type)
        ~name:(field_name Fields.currency)
      ++ value (optional Symbol.val_type)
        ~name:(field_name Fields.local_symbol))

  let execution_response () =
    Unpickler.Spec.(
      step (fun conv con_id symbol sec_type expiry strike right
             exchange currency local_symbol ->
             let contract = create
                 ?con_id ~sec_type ?expiry ?strike ?right ~exchange
                 ~currency ?local_symbol symbol
             in
             conv contract
           )
      ++ value (optional Contract_id.val_type)
        ~name:(field_name Fields.con_id)
      ++ value (required Symbol.val_type)
        ~name:(field_name Fields.symbol)
      ++ value (required string)
        ~name:(field_name Fields.sec_type)
      ++ value (optional date)
        ~name:(field_name Fields.expiry)
      ++ value (optional Price.val_type ~none_on_default:"0.0")
        ~name:(field_name Fields.strike)
      ++ value (optional Option_right.val_type)
        ~name:(field_name Fields.right)
      ++ value (required Exchange.val_type)
        ~name:(field_name Fields.exchange)
      ++ value (required Currency.val_type)
        ~name:(field_name Fields.currency)
      ++ value (optional Symbol.val_type)
        ~name:(field_name Fields.local_symbol))
end
