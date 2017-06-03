open Core

type ('a, 'b) t = Raw_order.t
  constraint 'a = [< Order_action.t ]
  constraint 'b = [< Order_type.t ]
  [@@deriving sexp]

let to_raw = Fn.id
let of_raw = Fn.id

let ( = ) t1 t2 = Raw_order.(=) (to_raw t1) (to_raw t2)

let order_type t = Order_type.t_of_tws t.Raw_order.order_type
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
