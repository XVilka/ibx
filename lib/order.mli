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
