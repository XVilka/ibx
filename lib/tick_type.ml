open Core_kernel
open Tws_prot

module T = struct
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
end
include T
include Sexpable.To_stringable (T)

let tws_of_t = function
  | `Option_volume -> "100"
  | `Option_open_interest -> "101"
  | `Historical_volatility -> "104"
  | `Implied_volatility -> "106"
  | `Index_future_premium -> "162"
  | `Misc_stats -> "165"
  | `Mark_price -> "221"
  | `Auction_values -> "225"
  | `Realtime_volume -> "233"
  | `Shortable -> "236"
  | `Inventory -> "256"
  | `Fundamental_ratios -> "258"
  | `Turn_off_market_data -> "mdoff"

let t_of_tws = function
  | "100" -> `Option_volume
  | "101" -> `Option_open_interest
  | "104" -> `Historical_volatility
  | "106" -> `Implied_volatility
  | "162" -> `Index_future_premium
  | "165" -> `Misc_stats
  | "221" -> `Mark_price
  | "225" -> `Auction_values
  | "233" -> `Realtime_volume
  | "236" -> `Shortable
  | "256" -> `Inventory
  | "258" -> `Fundamental_ratios
  | "mdoff" -> `Turn_off_market_data
  | s -> invalid_argf "Tick_type.t_of_tws: %s" s ()

let val_type = Val_type.create tws_of_t t_of_tws
