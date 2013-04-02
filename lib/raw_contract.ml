(* File: raw_contract.ml

   IBX - Pure OCaml implementation of the Interactive Brokers TWS API

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

module Type = struct
  type t = [ `Stock | `Futures | `Option | `Forex ] with sexp

  let to_string = function
    | `Stock   -> "STK"
    | `Futures -> "FUT"
    | `Option  -> "OPT"
    | `Forex   -> "CASH"

  let of_string = function
  	| "STK"  -> `Stock
	  | "FUT"  -> `Futures
	  | "OPT"  -> `Option
    | "CASH" -> `Forex
    | s -> invalid_argf "Type.of_string: %S" s ()

  let val_type = Val_type.create to_string of_string
end

module Security_id = struct
  include String
  module Type = struct
    type t = [ `ISIN | `CUSIP | `SEDOL | `RIC ] with sexp

    let to_string = function
      | `ISIN  -> "ISIN"
      | `CUSIP -> "CUSIP"
      | `SEDOL -> "SEDOL"
      | `RIC   -> "RIC"

    let of_string = function
      | "ISIN"  -> `ISIN
      | "CUSIP" -> `CUSIP
      | "SEDOL" -> `SEDOL
      | "RIC"   -> `RIC
      | s -> invalid_argf "Security_id.Type.of_string: %S" s ()

    let val_type = Val_type.create to_string of_string
  end

  let val_type = Val_type.create to_string of_string
end

module Option_right = struct
  type t = [ `Call | `Put ] with sexp

  let to_string = function
    | `Call -> "C"
    | `Put  -> "P"

  let of_string = function
    | "C" -> `Call
    | "P" -> `Put
    | s -> invalid_argf "Option_right.of_string: %S" s ()

  let val_type = Val_type.create to_string of_string
end

type t =
  { contract_id : Id.t option;
    symbol : Symbol.t;
    contract_type : string;
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
    ?(exchange=`SMART) ~currency ~contract_type symbol =
  { contract_id = id;
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
    ~contract_type:(use (=))
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
