module type S = sig
  type raw
  type ('a, 'b) t
    constraint 'a = [< Order_action.t ]
    constraint 'b = [< Order_type.t ]

  val of_raw : raw -> ('a, 'b) t
  val to_raw : ('a, 'b) t -> raw
end
