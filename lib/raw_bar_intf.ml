module type S = sig
  type raw
  type t
  val of_raw : raw -> t
  val to_raw : t -> raw
end
