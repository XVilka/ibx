(* File: query.mli

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

(** TWS queries *)

open Core
open Ibx_internal

(** {1 Connection and server} *)
(*****************************************************************************)

module Server_log_level : sig
  module Level : sig
    type t = [ `System | `Error | `Warning | `Information | `Detail ] [@@deriving sexp]
  end
  type t [@@deriving sexp]
  include Query_intf.S with type t := t
  val create : level:Level.t -> t
end

module Server_time : sig
  type t [@@deriving sexp]
  include Query_intf.S with type t := t
  val create : unit -> t
end

(** {1 Market data} *)
(*****************************************************************************)

module Market_data : sig
  type t [@@deriving sexp]
  include Query_intf.S with type t := t
  val create
    :  contract:[< Security_type.t ] Contract.t
    -> tick_types:Tick_type.t list
    -> snapshot:bool
    -> t
end

module Option_price : sig
  type t [@@deriving sexp]
  include Query_intf.S with type t := t
  val create
    :  contract:[ `Option ] Contract.t
    -> volatility:float
    -> underlying_price:Price.t
    -> t
end

module Implied_volatility : sig
  type t [@@deriving sexp]
  include Query_intf.S with type t := t
  val create
    :  contract:[ `Option ] Contract.t
    -> option_price:Price.t
    -> underlying_price:Price.t
    -> t
end

(** {1 Orders} *)
(*****************************************************************************)

module Submit_order : sig
  type t [@@deriving sexp]
  include Query_intf.S with type t := t
  val create
    :  contract:[< Security_type.t ] Contract.t
    -> order:([< Order_action.t ], [< Order_type.t ]) Order.t
    -> account_code:Account_code.t
    -> t
end

(** {1 Account and portfolio} *)
(*****************************************************************************)

module Account_updates : sig
  type t [@@deriving sexp]
  include Query_intf.S with type t := t
  val create
    :  subscribe:bool
    -> account_code:Account_code.t
    -> t
end

module Positions : sig
  type t [@@deriving sexp]
  include Query_intf.S with type t := t
  val create
    :  subscribe:bool
    -> account_code:Account_code.t
    -> t
end

(** {1 Executions} *)
(*****************************************************************************)

module Executions : sig
  type t [@@deriving sexp]
  include Query_intf.S with type t := t

  val create
    :  contract:[< Security_type.t] Contract.t
    -> client_id:Client_id.t
    -> account_code:Account_code.t
    -> time:Time.t
    -> action:Order_action.t
    -> t
end

(** {1 Contract details} *)
(*****************************************************************************)

module Contract_details : sig
  type t [@@deriving sexp]
  include Query_intf.S with type t := t

  val create
    :  ?con_id:Contract_id.t
    -> ?multiplier:int
    -> ?prim_exch:Exchange.t
    -> ?local_symbol:Symbol.t
    -> ?sec_id:Security_id.t
    -> ?include_expired:bool
    -> ?exchange:Exchange.t
    -> ?right:Option_right.t
    -> ?expiry:Date.t
    -> ?strike:Price.t
    -> sec_type:[< Security_type.t ]
    -> currency:Currency.t
    -> Symbol.t
    -> t
end

(** {1 Market depth} *)
(*****************************************************************************)

module Market_depth : sig
  type t [@@deriving sexp]
  include Query_intf.S with type t := t
  val create
    :  contract:[< Security_type.t ] Contract.t
    -> num_rows:int
    -> t
end

(** {1 Historical data} *)
(*****************************************************************************)

module History : sig
  module Tick_type : sig
    type t =
      [ `Trades
      | `Midpoint
      | `Bid
      | `Ask
      | `Bid_ask
      | `Historical_volatility
      | `Implied_volatility
      | `Option_volume
      ] [@@deriving sexp]
    include Stringable.S with type t := t
  end

  type t [@@deriving sexp]
  include Query_intf.S with type t := t
  val create
    :  contract:[< Security_type.t ] Contract.t
    -> until:Time.t
    -> bar_size:Bar.Size.t
    -> duration:Bar.Duration.t
    -> use_rth:bool
    -> tick_type:Tick_type.t
    -> t
end

(** {1 Realtime bars} *)
(*****************************************************************************)

module Realtime_bars : sig
  module Tick_type : sig
    type t = [ `Trades | `Midpoint | `Bid | `Ask ]
  end

  type t [@@deriving sexp]
  include Query_intf.S with type t := t
  val create
    :  contract:[< Security_type.t ] Contract.t
    -> tick_type:Tick_type.t
    -> use_rth:bool
    -> t
end
