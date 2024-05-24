open Core

type t [@@deriving sexp, eq]
include Twsable.S with type t := t

val create : date:Date.t -> hours:Time_float_unix.Ofday.t list -> t

val date : t -> Date.t
val closed : t -> bool

val start : t -> zone:Time_float_unix.Zone.t -> Time_float_unix.t option
val stop  : t -> zone:Time_float_unix.Zone.t -> Time_float_unix.t option

val start_exn : t -> zone:Time_float_unix.Zone.t -> Time_float_unix.t
val stop_exn  : t -> zone:Time_float_unix.Zone.t -> Time_float_unix.t
