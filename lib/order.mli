(* File: order.mli

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
  type t = Raw_order.Id.t with sexp
  include Unique_id.Id with type t := t
end

module Action : sig
  type t = [ `Buy | `Sell | `Sell_short ] with sexp
end

module Type : sig
  type t = [ `MKT | `LMT ] with sexp
  include Stringable.S with type t := t
end

type ('a, 'b) t
constraint 'a = [< Action.t ]
constraint 'b = [< Type.t ]
with sexp

include Raw_order_intf.S
  with type raw := Raw_order.t
  with type ('a, 'b) t := ('a, 'b) t

val ( = ) : ('a, 'b) t -> ('a, 'b) t -> bool

val order_type : ('a, 'b) t -> [> Type.t ]
val quantity   : ('a, 'b) t -> int

val buy_limit
  :  quantity:int
  -> Price.t
  -> ([> `Buy ], [> `LMT ]) t

val sell_limit
  :  quantity:int
  -> Price.t
  -> ([> `Sell ], [> `LMT ]) t

val buy_market
  :  quantity:int
  -> ([> `Buy  ], [> `MKT ]) t

val sell_market
  :  quantity:int
  -> ([> `Sell ], [> `MKT ]) t
