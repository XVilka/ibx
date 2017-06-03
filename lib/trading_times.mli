open Core

type t [@@deriving sexp]
include Twsable.S with type t := t

val create : date:Date.t -> hours:Time.Ofday.t list -> t

val date : t -> Date.t
val closed : t -> bool

val start : t -> zone:Time.Zone.t -> Time.t option
val stop  : t -> zone:Time.Zone.t -> Time.t option

val start_exn : t -> zone:Time.Zone.t -> Time.t
val stop_exn  : t -> zone:Time.Zone.t -> Time.t
