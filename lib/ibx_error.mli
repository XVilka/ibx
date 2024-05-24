open Core

include module type of struct include Protocol.Ibx_error end

exception Ibx of t

val raise : t -> 'a

val to_error : t -> Error.t
