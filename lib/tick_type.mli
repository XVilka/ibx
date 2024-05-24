open Core

type t =
  [ `Auction_values (* Volume, price and imbalance *)
  | `Fundamental_ratios
  | `Historical_volatility
  | `Implied_volatility
  | `Index_future_premium
  | `Inventory
  | `Mark_price (* Used in TWS P&L computations *)
  | `Misc_stats
  | `Option_open_interest
  | `Option_volume
  | `Realtime_volume
  | `Shortable
  | `Turn_off_market_data
  ] [@@deriving sexp, eq]
include Stringable.S with type t := t
include Twsable.S with type t := t
