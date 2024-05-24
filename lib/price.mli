open Core

type t = private float [@@deriving sexp]
include Floatable.S with type t := t
include Identifiable.S with type t := t
include Robustly_comparable.S with type t := t
include Twsable.S with type t := t

val (+) : t -> t -> t
val (-) : t -> t -> t
val ( * ) : t -> t -> t
val (/) : t -> t -> t

val zero : t
val one : t
val nan : t

val add : t -> t -> t
val sub : t -> t -> t
val neg : t -> t
val scale : t -> t -> t
val abs : t -> t
val is_nan : t -> bool
