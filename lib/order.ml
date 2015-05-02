(* File: order.ml

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

include struct
  open Raw_order
  module Action = Action
  module Type = Type
end

type ('a, 'b) t = Raw_order.t
constraint 'a = [< Action.t ]
constraint 'b = [< Type.t ]
with sexp

let to_raw = Fn.id
let of_raw = Fn.id

let ( = ) t1 t2 = Raw_order.(=) (to_raw t1) (to_raw t2)

let order_type t = Type.t_of_tws t.Raw_order.order_type
let quantity t = t.Raw_order.quantity

let buy_limit ~quantity limit_price =
  of_raw (
    Raw_order.create
      ~action:"BUY"
      ~order_type:"LMT"
      ~quantity
      ~limit_price
      ()
  )

let sell_limit ~quantity limit_price =
  of_raw (
    Raw_order.create
      ~action:"SELL"
      ~order_type:"LMT"
      ~quantity
      ~limit_price
      ()
  )

let buy_market ~quantity =
  of_raw (
    Raw_order.create
      ~action:"BUY"
      ~order_type:"MKT"
      ~quantity
      ()
  )

let sell_market ~quantity =
  of_raw (
    Raw_order.create
      ~action:"SELL"
      ~order_type:"MKT"
      ~quantity
      ()
  )
