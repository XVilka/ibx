open Rg.R

let account_updates  = lazy (account_updates_g ())
let book_updates     = lazy (book_updates_g ())
let contract_details = lazy (contract_details_g ())
let executions       = lazy (executions_g ())
let history          = lazy (history_g ())
let market_data      = lazy (market_data_g ())
let order_states     = lazy (order_states_g ())
let positions        = lazy (positions_g ())
let realtime_bars    = lazy (realtime_bars_g ())
let server_time      = lazy (server_time_g ())
let tick_option      = lazy (tick_option_g ())
