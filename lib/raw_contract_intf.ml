module type S = sig
  type raw
  type 'a t constraint 'a = [< Security_type.t ]
  val of_raw : raw -> 'a t
  val to_raw : 'a t -> raw
end
