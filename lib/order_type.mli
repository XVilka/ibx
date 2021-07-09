open Core_kernel

type t =
  [ `Auction
  | `Basket
  | `Bracket
  | `Hidden
  | `Iceberg
  | `Limit
  | `Limit_if_touched
  | `Limit_on_close
  | `Limit_on_open
  | `Market
  | `Market_if_touched
  | `Market_on_close
  | `Market_on_open
  | `Market_to_limit
  | `Pegged_to_benchmark
  | `Pegged_to_market
  | `Pegged_to_midpoint
  | `Stop
  | `Stop_limit
  | `TWAP
  | `Trailing_limit_if_touched
  | `Trailing_market_if_touched
  | `Trailing_stop
  | `Trailing_stop_limit
  | `VWAP
  ] [@@deriving sexp, eq]
include Stringable.S with type t := t
include Twsable.S with type t := t
