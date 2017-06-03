open Core

type t =
  [ `Buy
  | `Sell
  | `Sell_short
  ] [@@deriving sexp]
include Stringable.S with type t := t
include Twsable.S with type t := t
