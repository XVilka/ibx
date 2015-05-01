(* File: contract.mli

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

module Id : sig
  type t = Raw_contract.Id.t with sexp
  include Unique_id.Id with type t := t
end

module Security_type : sig
  type t =
  [ `Stock
  | `Index
  | `Futures
  | `Option
  | `Fut_opt
  | `Forex
  ] with sexp
  include Stringable.S with type t := t
end

module Option_right : sig
  type t = [ `Call | `Put ] with sexp
  include Stringable.S with type t := t
end

module Security_id : sig
  type t =
  [ `ISIN  of string
  | `RIC   of string
  | `CUSIP of string
  | `SEDOL of string
  ] with sexp
  include Stringable.S with type t := t
end

type 'a t
constraint 'a = [< Security_type.t ] with sexp

include Raw_contract_intf.S
  with type raw := Raw_contract.t
  with type 'a t := 'a t

val security_type    : 'a t -> [> Security_type.t ]
val id               : 'a t -> Id.t option
val symbol           : 'a t -> Symbol.t
val exchange         : 'a t -> Exchange.t
val listed_on        : 'a t -> Exchange.t option
val currency         : 'a t -> Currency.t
val local_symbol     : 'a t -> Symbol.t option
val security_id      : 'a t ->  Security_id.t option
val underlying       : [< `Option | `Fut_opt ] t -> [> `Stock | `Futures ] t
val strike           : [< `Option | `Fut_opt ] t -> Price.t
val option_right     : [< `Option | `Fut_opt ] t -> Option_right.t
val expiry           : [< `Futures | `Option | `Fut_opt ] t -> Date.t
val days_to_expiry   : [< `Futures | `Option | `Fut_opt ] t -> zone:Time.Zone.t -> int
val multiplier       : [< `Futures | `Option | `Fut_opt | `Futures ] t -> int
val include_expired  : [ `Futures ] t -> bool
val combo_legs       : 'a t -> int

val to_string : 'a t -> string

val ( = ) : 'a t -> 'a t -> bool

val stock
  :  ?id:Id.t
  -> ?listed_on:Exchange.t
  -> ?local_symbol:Symbol.t
  -> ?security_id:Security_id.t
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> Symbol.t
  -> [> `Stock ] t

val index
  :  ?id:Id.t
  -> ?local_symbol:Symbol.t
  -> ?security_id:Security_id.t
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> Symbol.t
  -> [> `Index ] t

val futures
  :  ?id:Id.t
  -> ?multiplier:int
  -> ?local_symbol:Symbol.t
  -> ?security_id:Security_id.t
  -> ?include_expired:bool
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> expiry:Date.t
  -> Symbol.t
  -> [> `Futures ] t

val option
  :  ?id:Id.t
  -> ?multiplier:int
  -> ?local_symbol:Symbol.t
  -> ?security_id:Security_id.t
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> option_right:Option_right.t
  -> expiry:Date.t
  -> strike:Price.t
  -> Symbol.t
  -> [> `Option ] t

val futures_option
  :  ?id:Id.t
  -> ?multiplier:int
  -> ?local_symbol:Symbol.t
  -> ?security_id:Security_id.t
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> option_right:Option_right.t
  -> expiry:Date.t
  -> strike:Price.t
  -> Symbol.t
  -> [> `Fut_opt ] t

val forex
  :  ?id:Id.t
  -> ?local_symbol:Symbol.t
  -> ?security_id:Security_id.t
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> Symbol.t
  -> [> `Forex ] t
