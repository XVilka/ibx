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

type 'a t
constraint 'a = [< Security_type.t ] with sexp
(** A contract belonging to a security type like stock, futures, option etc. *)

include Raw_contract_intf.S
  with type raw := Raw_contract.t
  with type 'a t := 'a t

val sec_type : 'a t -> Security_type.t
(** The underlying asset belongs to the returned security type. *)

val con_id : 'a t -> Contract_id.t option
(** Returns the unique contract ID or None if unknown. *)

val symbol : 'a t -> Symbol.t
(** Returns the symbol of the underlying asset. *)

val exchange : 'a t -> Exchange.t
(** Returns the exchange of the order destination. *)

val listed_on : 'a t -> Exchange.t option
(** Returns the listing exchange of the underlying asset or None if unknown. *)

val currency : 'a t -> Currency.t
(** The underlying asset is traded in the returned currency. *)

val local_symbol : 'a t -> Symbol.t option
(** Returns the local exchange of the underlying asset or None if unknown. *)

val sec_id : 'a t -> Security_id.t option
(** Returns the security ID of the underlying asset or None if unknown. *)

val underlying : [< `Option | `Fut_opt ] t -> [> `Stock | `Futures ] t
(** Returns the underlying stock or futures contract of an option. *)

val strike : [< `Option | `Fut_opt ] t -> Price.t
(** Returns the strike price of an option contract. *)

val right : [< `Option | `Fut_opt ] t -> Option_right.t
(** Returns the right (Put or Call) of an option contract. *)

val expiry : [< `Futures | `Option | `Fut_opt ] t -> Date.t
(** Returns the expiry date of futures and option contracts. *)

val sort_futures_chain :
  [< `Futures ] t list -> [> `Futures ] t list
(** Sorts a futures chain in ascending order by expiry. *)

val sort_option_chain :
  [< `Option | `Fut_opt ] t list -> [> `Option | `Fut_opt ] t list
(** Sorts an option chain in ascending order by expiry and strike price. *)

val days_to_expiry
  :  [< `Futures | `Option | `Fut_opt ] t
  -> zone:Time.Zone.t
  -> int
(** Returns the number of days until a futures or option contract expires *)

val multiplier : [< `Futures | `Option | `Fut_opt ] t -> int
(** Returns the contract multiplier of a futures or option contract. *)

val to_string : 'a t -> string

(** Checks equality of two contracts. *)
val ( = ) : 'a t -> 'a t -> bool

(** Creates a new stock contract. *)
val stock
  :  ?con_id:Contract_id.t
  -> ?listed_on:Exchange.t
  -> ?local_symbol:Symbol.t
  -> ?sec_id:Security_id.t
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> Symbol.t
  -> [> `Stock ] t

(** Creates a new contract representing an index. *)
val index
  :  ?con_id:Contract_id.t
  -> ?local_symbol:Symbol.t
  -> ?sec_id:Security_id.t
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> Symbol.t
  -> [> `Index ] t

(** Creates a new futures contract. *)
val futures
  :  ?con_id:Contract_id.t
  -> ?multiplier:int
  -> ?local_symbol:Symbol.t
  -> ?sec_id:Security_id.t
  -> ?include_expired:bool
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> expiry:Date.t
  -> Symbol.t
  -> [> `Futures ] t

(** Creates a new option contract. *)
val option
  :  ?con_id:Contract_id.t
  -> ?multiplier:int
  -> ?local_symbol:Symbol.t
  -> ?sec_id:Security_id.t
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> right:Option_right.t
  -> expiry:Date.t
  -> strike:Price.t
  -> Symbol.t
  -> [> `Option ] t

(** Creates a new futures option contract. *)
val futures_option
  :  ?con_id:Contract_id.t
  -> ?multiplier:int
  -> ?local_symbol:Symbol.t
  -> ?sec_id:Security_id.t
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> right:Option_right.t
  -> expiry:Date.t
  -> strike:Price.t
  -> Symbol.t
  -> [> `Fut_opt ] t

(** Creates a new forex contract. *)
val forex
  :  ?con_id:Contract_id.t
  -> ?local_symbol:Symbol.t
  -> ?sec_id:Security_id.t
  -> ?exchange:Exchange.t
  -> currency:Currency.t
  -> Symbol.t
  -> [> `Forex ] t
