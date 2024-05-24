open Core

type t [@@deriving sexp]
include Twsable.S with type t := t
include Stringable.S with type t := t

val create : unit -> t

val default : t

val increase : t -> int -> t

val to_int_exn : t -> int

val of_int_exn : int -> t
