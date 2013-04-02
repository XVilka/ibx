(* File: contract.mli

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

module Id : sig
  type t = Raw_contract.Id.t with sexp
  include Unique_id.Id with type t := t
end

module Type : sig
  type t = [ `Stock | `Futures | `Option | `Forex ] with sexp
end

type 'a t
constraint 'a = [< Type.t ] with sexp

include Raw_contract_intf.S
  with type raw := Raw_contract.t
  with type 'a t := 'a t

val id           : 'a t -> Id.t option
val symbol       : 'a t -> Symbol.t
val exchange     : 'a t -> Exchange.t
val listing_exchange : 'a t -> Exchange.t option
val currency     : 'a t -> Currency.t
val local_symbol : 'a t -> Symbol.t option
val security_id  : 'a t -> [ `ISIN  of string
                           | `RIC   of string
                           | `CUSIP of string
                           | `SEDOL of string ] option

val strike       : [ `Option ] t -> Price.t
val option_right : [ `Option ] t -> [ `Call | `Put ]
val expiry       : [ `Option | `Futures ] t -> Date.t
val multiplier   : [ `Option | `Futures ] t -> string
val include_expired : [ `Futures ] t -> bool
val combo_legs : 'a t -> int

val ( = ) : 'a t -> 'a t -> bool

val stock :
  ?id:Id.t
  -> ?listing_exchange:Exchange.t
  -> ?local_symbol:Symbol.t
  -> ?security_id:[ `ISIN  of string
                  | `RIC   of string
                  | `CUSIP of string
                  | `SEDOL of string ]
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> Symbol.t
  -> [> `Stock ] t

val futures :
  ?id:Id.t
  -> ?multiplier:string
  -> ?listing_exchange:Exchange.t
  -> ?local_symbol:Symbol.t
  -> ?security_id:[ `ISIN  of string
                  | `RIC   of string
                  | `CUSIP of string
                  | `SEDOL of string ]
  -> ?include_expired:bool
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> expiry:Date.t
  -> Symbol.t
  -> [> `Futures ] t

val option :
  ?id:Id.t
  -> ?multiplier:string
  -> ?listing_exchange:Exchange.t
  -> ?local_symbol:Symbol.t
  -> ?security_id:[ `ISIN  of string
                  | `RIC   of string
                  | `CUSIP of string
                  | `SEDOL of string ]
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> option_right:[ `Call | `Put ]
  -> expiry:Date.t
  -> strike:Price.t
  -> Symbol.t
  -> [> `Option ] t

val forex :
  ?id:Id.t
  -> ?listing_exchange:Exchange.t
  -> ?local_symbol:Symbol.t
  -> ?security_id:[ `ISIN  of string
                  | `RIC   of string
                  | `CUSIP of string
                  | `SEDOL of string ]
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> Symbol.t
  -> [> `Forex ] t
