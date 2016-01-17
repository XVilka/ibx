(* File: order.mli

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

type ('a, 'b) t
constraint 'a = [< Order_action.t ]
constraint 'b = [< Order_type.t ]
[@@deriving sexp]

include Raw_order_intf.S
  with type raw := Raw_order.t
  with type ('a, 'b) t := ('a, 'b) t

val ( = ) : ('a, 'b) t -> ('a, 'b) t -> bool

val order_type : ('a, 'b) t -> Order_type.t
val quantity   : ('a, 'b) t -> Volume.t

val buy_limit
  :  quantity:Volume.t
  -> Price.t
  -> ([> `Buy ], [> `Limit ]) t

val sell_limit
  :  quantity:Volume.t
  -> Price.t
  -> ([> `Sell ], [> `Limit ]) t

val buy_market
  :  quantity:Volume.t
  -> ([> `Buy  ], [> `Market ]) t

val sell_market
  :  quantity:Volume.t
  -> ([> `Sell ], [> `Market ]) t
