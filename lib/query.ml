open Core
open Tws_prot

module Unit (Arg : sig val name:string end) = struct
  type t = unit [@@deriving sexp, eq]
  let create () = ()
  let ( = ) t1 t2 = equal t1 t2
  let encoder =
    Encoder.create ~name:Arg.name
      Encoder.Spec.(value (required unit))
  let decoder = Only_in_test.of_thunk (fun () ->
    Decoder.create ~name:Arg.name
      Decoder.Spec.(value (required unit) ~name:"unit")
      Fn.id)
end

(* +-----------------------------------------------------------------------+
   | Connection and server                                                 |
   +-----------------------------------------------------------------------+ *)

module Server_log_level = struct
  module Level = struct
    type t =
      [ `System
      | `Error
      | `Warning
      | `Information
      | `Detail
      ] [@@deriving sexp, eq]

    let tws_of_t = function
      | `System -> "1"
      | `Error -> "2"
      | `Warning -> "3"
      | `Information -> "4"
      | `Detail -> "5"

    let t_of_tws = function
      | "1" -> `System
      | "2" -> `Error
      | "3" -> `Warning
      | "4" -> `Information
      | "5" -> `Detail
      | s   -> invalid_argf "Level.t_of_tws: %s" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t = Level.t [@@deriving sexp]

  let create ~level = level

  let ( = ) t1 t2 = Level.equal t1 t2

  let encoder =
    Encoder.create ~name:"Query.Server_log_level"
      Encoder.Spec.(value (required Level.val_type))

  let decoder = Only_in_test.of_thunk (fun () ->
    Decoder.create ~name:"Query.Server_log_level"
      Decoder.Spec.(value (required Level.val_type) ~name:"log_level")
      Fn.id)
end

module Server_time = Unit (struct
  let name = "Query.Server_time"
end)

(* +-----------------------------------------------------------------------+
   | Market data                                                           |
   +-----------------------------------------------------------------------+ *)

module Market_data = struct
  type t =
    { contract : Raw_contract.t
    ; tick_types : Tick_type.t list
    ; snapshot : bool
    } [@@deriving sexp, fields]

  let create ~contract ~tick_types ~snapshot =
    { contract = Contract.to_raw contract
    ; tick_types
    ; snapshot
    }

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~contract:(use Raw_contract.(=))
      ~tick_types:(use (List.equal Tick_type.equal))
      ~snapshot:(use Bool.(=))

  let encoder =
    let contract_spec =
      Raw_contract.Encoder_specs.market_data_query ()
    in
    Encoder.create ~name:"Query.Market_data"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~tick_types:(fields_value (sequence Tick_type.val_type))
            ~snapshot:(fields_value (required bool)))
          (fun { contract; tick_types; snapshot } ->
             `Args $ contract $ tick_types $ snapshot))

  let decoder = Only_in_test.of_thunk (fun () ->
    let contract_spec =
      Raw_contract.Decoder_specs.market_data_query ()
    in
    Decoder.create ~name:"Query.Market_data"
      Decoder.Spec.(
        Fields.fold
          ~init:(empty ())
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~tick_types:(fields_value (sequence Tick_type.val_type))
          ~snapshot:(fields_value (required bool)))
      (fun contract tick_types snapshot ->
         { contract; tick_types; snapshot }))
end

module Option_price = struct
  type t =
    { contract : Raw_contract.t
    ; volatility : float
    ; underlying_price : Price.t
    } [@@deriving sexp, fields]

  let create ~contract ~volatility ~underlying_price =
    { contract = Contract.to_raw contract
    ; volatility
    ; underlying_price
    }

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~contract:(use Raw_contract.(=))
      ~volatility:(use Float.(=.))
      ~underlying_price:(use Price.(=.))

  let encoder =
    let contract_spec =
      Raw_contract.Encoder_specs.common_option_calc ()
    in
    Encoder.create ~name:"Query.Option_price"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~volatility:(fields_value (required float))
            ~underlying_price:(fields_value (required Price.val_type)))
          (fun { contract; volatility; underlying_price } ->
             `Args $ contract $ volatility $ underlying_price))

  let decoder = Only_in_test.of_thunk (fun () ->
    let contract_spec =
      Raw_contract.Decoder_specs.option_price_query ()
    in
    Decoder.create ~name:"Query.Option_price"
      Decoder.Spec.(
        Fields.fold
          ~init:(empty ())
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~volatility:(fields_value (required float))
          ~underlying_price:(fields_value (required Price.val_type)))
      (fun contract volatility underlying_price ->
         { contract; volatility; underlying_price }))
end

module Implied_volatility = struct
  type t =
    { contract : Raw_contract.t
    ; option_price : Price.t
    ; underlying_price : Price.t
    } [@@deriving sexp, fields]

  let create ~contract ~option_price ~underlying_price =
    { contract = Contract.to_raw contract
    ; option_price
    ; underlying_price
    }

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~contract:(use Raw_contract.(=))
      ~option_price:(use Price.(=.))
      ~underlying_price:(use Price.(=.))

  let encoder =
    let contract_spec =
      Raw_contract.Encoder_specs.common_option_calc ()
    in
    Encoder.create ~name:"Query.Implied_volatility"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~option_price:(fields_value (required Price.val_type))
            ~underlying_price:(fields_value (required Price.val_type)))
          (fun { contract; option_price; underlying_price } ->
             `Args $ contract $ option_price $ underlying_price))

  let decoder = Only_in_test.of_thunk (fun () ->
    let contract_spec =
      Raw_contract.Decoder_specs.implied_volatility_query ()
    in
    Decoder.create ~name:"Query.Implied_volatility"
      Decoder.Spec.(
        Fields.fold
          ~init:(empty ())
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~option_price:(fields_value (required Price.val_type))
          ~underlying_price:(fields_value (required Price.val_type)))
      (fun contract option_price underlying_price ->
         { contract; option_price; underlying_price }))
end


(* +-----------------------------------------------------------------------+
   | Orders                                                                |
   +-----------------------------------------------------------------------+ *)

module Submit_order = Submit_order

(* +-----------------------------------------------------------------------+
   | Account and portfolio                                                 |
   +-----------------------------------------------------------------------+ *)

module Updates (Arg : sig val name:string end) = struct
  type t =
    { subscribe : bool
    ; account_code : Account_code.t
    } [@@deriving sexp, fields]

  let create = Fields.create

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~subscribe:(use Bool.(=))
      ~account_code:(use Account_code.(=))

  let encoder =
    Encoder.create ~name:Arg.name
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~subscribe:(fields_value (required bool))
            ~account_code:(fields_value (required Account_code.val_type)))
          (fun t -> `Args $ t.subscribe $ t.account_code))

  let decoder = Only_in_test.of_thunk (fun () ->
    Decoder.create ~name:Arg.name
      Decoder.Spec.(
        Fields.fold
          ~init:(empty ())
          ~subscribe:(fields_value (required bool))
          ~account_code:(fields_value (required Account_code.val_type)))
      (fun subscribe account_code -> { subscribe; account_code }))
end

module Account_updates = Updates (struct
  let name = "Query.Account_updates"
end)

module Positions = Updates (struct
  let name = "Query.Positions"
end)

(* +-----------------------------------------------------------------------+
   | Executions                                                            |
   +-----------------------------------------------------------------------+ *)

module Executions = struct
  module Time = struct
    include Time_float_unix
    let tws_of_t tm = Time_float_unix.format tm "%Y%m%d-%H:%M:%S" ~zone:(Lazy.force Time_float_unix.Zone.local)
    let t_of_tws s = Time_float_unix.of_string (String.tr ~target:'-' ~replacement:' ' s)
    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t =
    { client_id : Client_id.t
    ; account_code : Account_code.t
    ; time : Time_float_unix.t
    ; symbol : Symbol.t
    ; sec_type : string
    ; exchange : Exchange.t
    ; action : Order_action.t
    } [@@deriving sexp, fields]

  let create ~contract ~client_id ~account_code ~time ~action =
    let contract = Contract.to_raw contract in
    { client_id
    ; account_code
    ; time
    ; symbol = Raw_contract.symbol contract
    ; sec_type = Raw_contract.sec_type contract
    ; exchange = Raw_contract.exchange contract
    ; action
    }

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~client_id:(use Client_id.(=))
      ~account_code:(use Account_code.(=))
      ~time:(use Time_float_unix.(=))
      ~symbol:(use Symbol.(=))
      ~sec_type:(use String.(=))
      ~exchange:(use Exchange.equal)
      ~action:(use Order_action.equal)

  let encoder =
    Encoder.create ~name:"Query.Executions"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~client_id:(fields_value (required Client_id.val_type))
            ~account_code:(fields_value (required Account_code.val_type))
            ~time:(fields_value (required Time.val_type))
            ~symbol:(fields_value (required Symbol.val_type))
            ~sec_type:(fields_value (required string))
            ~exchange:(fields_value (required Exchange.val_type))
            ~action:(fields_value (required Order_action.val_type)))
          (fun t ->
             `Args
             $ t.client_id
             $ t.account_code
             $ t.time
             $ t.symbol
             $ t.sec_type
             $ t.exchange
             $ t.action))

  let decoder = Only_in_test.of_thunk (fun () ->
    Decoder.create ~name:"Query.Executions"
      Decoder.Spec.(
        Fields.fold
          ~init:(empty ())
          ~client_id:(fields_value (required Client_id.val_type))
          ~account_code:(fields_value (required Account_code.val_type))
          ~time:(fields_value (required Time.val_type))
          ~symbol:(fields_value (required Symbol.val_type))
          ~sec_type:(fields_value (required string))
          ~exchange:(fields_value (required Exchange.val_type))
          ~action:(fields_value (required Order_action.val_type)))
      (fun client_id account_code time symbol sec_type exchange action ->
         { client_id
         ; account_code
         ; time
         ; symbol
         ; sec_type
         ; exchange
         ; action
         }))
end

(* +-----------------------------------------------------------------------+
   | Contract details                                                      |
   +-----------------------------------------------------------------------+ *)

module Contract_details = struct
  type t = Raw_contract.t [@@deriving sexp]

  let create ?con_id ?multiplier ?prim_exch ?local_symbol ?sec_id
      ?include_expired ?exchange ?right ?expiry ?strike ~sec_type ~currency
      symbol =
    Raw_contract.create
      ?con_id
      ?multiplier
      ?prim_exch
      ?local_symbol
      ?sec_id_type:(Option.map sec_id ~f:Security_id.sec_id_type)
      ?sec_id:(Option.map sec_id ~f:Security_id.sec_id)
      ?include_expired
      ?exchange
      ?expiry
      ?strike
      ?right
      ~currency
      ~sec_type:(Security_type.tws_of_t sec_type)
      symbol

  let ( = ) t1 t2 = Raw_contract.(=) t1 t2

  let encoder =
    Encoder.create ~name:"Query.Contract_details"
      (Raw_contract.Encoder_specs.contract_details_query ())

  let decoder = Only_in_test.of_thunk (fun () ->
    Decoder.create ~name:"Query.Contract_details"
      (Raw_contract.Decoder_specs.contract_details_query ())
      Fn.id)
end

(* +-----------------------------------------------------------------------+
   | Market depth                                                          |
   +-----------------------------------------------------------------------+ *)

module Market_depth = struct
  type t =
    { contract : Raw_contract.t
    ; num_rows : int
    } [@@deriving sexp, fields]

  let create ~contract ~num_rows =
    { contract = Contract.to_raw contract
    ; num_rows
    }

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~contract:(use Raw_contract.(=))
      ~num_rows:(use (=))

  let encoder =
    let contract_spec =
      Raw_contract.Encoder_specs.market_depth_query ()
    in
    Encoder.create ~name:"Query.Market_depth"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~num_rows:(fields_value (required int)))
          (fun { contract; num_rows } -> `Args $ contract $ num_rows ))

  let decoder = Only_in_test.of_thunk (fun () ->
    let contract_spec =
      Raw_contract.Decoder_specs.market_depth_query ()
    in
    Decoder.create ~name:"Query.Market_depth"
      Decoder.Spec.(
        Fields.fold
          ~init:(empty ())
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~num_rows:(fields_value (required int)))
      (fun contract num_rows -> { contract; num_rows }))
end

(* +-----------------------------------------------------------------------+
   | Historical data                                                       |
   +-----------------------------------------------------------------------+ *)

module History = struct
  module Tick_type = struct
    module T = struct
      type t =
        [ `Trades
        | `Midpoint
        | `Bid
        | `Ask
        | `Bid_ask
        | `Historical_volatility
        | `Implied_volatility
        | `Option_volume
        ] [@@deriving sexp, eq]
    end
    include T
    include Sexpable.To_stringable (T)

    let tws_of_t = function
      | `Trades -> "TRADES"
      | `Midpoint -> "MIDPOINT"
      | `Bid -> "BID"
      | `Ask -> "ASK"
      | `Bid_ask -> "BID_ASK"
      | `Historical_volatility -> "HISTORICAL_VOLATILITY"
      | `Implied_volatility -> "OPTION_IMPLIED_VOLATILITY"
      | `Option_volume -> "OPTION_VOLUME"

    let t_of_tws = function
      | "TRADES" -> `Trades
      | "MIDPOINT" -> `Midpoint
      | "BID" -> `Bid
      | "ASK" -> `Ask
      | "BID_ASK" -> `Bid_ask
      | "HISTORICAL_VOLATILITY" -> `Historical_volatility
      | "OPTION_IMPLIED_VOLATILITY" -> `Implied_volatility
      | "OPTION_VOLUME" -> `Option_volume
      | s -> invalid_argf "Tick_type.t_of_tws: %S" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t =
    { contract : Raw_contract.t
    ; until : Time_float_unix.t
    ; bar_size : Bar.Size.t
    ; duration : Bar.Duration.t
    ; use_rth : bool
    ; tick_type : Tick_type.t
    ; date_format : string
    } [@@deriving sexp, fields]

  let create ~contract ~until ~bar_size ~duration ~use_rth ~tick_type =
    { contract = Contract.to_raw contract
    ; until
    ; bar_size
    ; duration
    ; use_rth
    ; tick_type
    ; date_format = "1"
    }

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~contract:(use Raw_contract.(=))
      ~until:(use Time_float_unix.(=))
      ~bar_size:(use Bar.Size.equal)
      ~duration:(use Bar.Duration.equal)
      ~use_rth:(use Bool.(=))
      ~tick_type:(use Tick_type.equal)
      ~date_format:(use String.(=))

  let encoder =
    let contract_spec = Raw_contract.Encoder_specs.history_query () in
    Encoder.create ~name:"Query.History"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~until:(fields_value (required time))
            ~bar_size:(fields_value (required Bar.Size.val_type))
            ~duration:(fields_value (required Bar.Duration.val_type))
            ~use_rth:(fields_value (required bool))
            ~tick_type:(fields_value (required Tick_type.val_type))
            ~date_format:(fields_value (required string)))
          (fun t ->
             `Args
             $ t.contract
             $ t.until
             $ t.bar_size
             $ t.duration
             $ t.use_rth
             $ t.tick_type
             $ t.date_format))

  let decoder = Only_in_test.of_thunk (fun () ->
    let contract_spec = Raw_contract.Decoder_specs.history_query () in
    Decoder.create ~name:"Query.History"
      Decoder.Spec.(
        Fields.fold
          ~init:(empty ())
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~until:(fields_value (required time))
          ~bar_size:(fields_value (required Bar.Size.val_type))
          ~duration:(fields_value (required Bar.Duration.val_type))
          ~use_rth:(fields_value (required bool))
          ~tick_type:(fields_value (required Tick_type.val_type))
          ~date_format:(fields_value (required string)))
      (fun contract until bar_size duration use_rth tick_type date_format ->
         { contract
         ; until
         ; bar_size
         ; duration
         ; use_rth
         ; tick_type
         ; date_format
         }))
end

(* +-----------------------------------------------------------------------+
   | Realtime bars                                                         |
   +-----------------------------------------------------------------------+ *)

module Realtime_bars = struct
  module Bar_size = struct
    type t =
      [ `Five_sec
      ] [@@deriving sexp, eq]

    let tws_of_t = function
      | `Five_sec -> "5"

    let t_of_tws = function
      | "5" -> `Five_sec
      | s -> invalid_argf "Bar_size.t_of_tws: %S" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  module Tick_type = struct
    type t =
      [ `Trades
      | `Midpoint
      | `Bid
      | `Ask
      ] [@@deriving sexp, eq]

    let tws_of_t = function
      | `Trades -> "TRADES"
      | `Bid -> "BID"
      | `Ask -> "ASK"
      | `Midpoint -> "MIDPOINT"

    let t_of_tws = function
      | "TRADES" -> `Trades
      | "BID" -> `Bid
      | "ASK" -> `Ask
      | "MIDPOINT" -> `Midpoint
      | s -> invalid_argf "Tick_type.t_of_tws: %s" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t =
    { contract : Raw_contract.t
    ; bar_size : Bar_size.t
    ; tick_type : Tick_type.t
    ; use_rth : bool
    } [@@deriving sexp, fields]

  let create ~contract ~tick_type ~use_rth =
    { contract = Contract.to_raw contract
    ; bar_size = `Five_sec
    ; tick_type
    ; use_rth
    }

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~contract:(use Raw_contract.(=))
      ~bar_size:(use Bar_size.equal)
      ~tick_type:(use Tick_type.equal)
      ~use_rth:(use Bool.(=))

  let encoder =
    let contract_spec =
      Raw_contract.Encoder_specs.realtime_bars_query ()
    in
    Encoder.create ~name:"Query.Realtime_bars"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~bar_size:(fields_value (required Bar_size.val_type))
            ~tick_type:(fields_value (required Tick_type.val_type))
            ~use_rth:(fields_value (required bool)))
          (fun { contract; bar_size; tick_type; use_rth } ->
             `Args $ contract $ bar_size $ tick_type $ use_rth ))

  let decoder = Only_in_test.of_thunk (fun () ->
    let contract_spec =
      Raw_contract.Decoder_specs.realtime_bars_query ()
    in
    Decoder.create ~name:"Query.Realtime_bars"
      Decoder.Spec.(
        Fields.fold
          ~init:(empty ())
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~bar_size:(fields_value (required Bar_size.val_type))
          ~tick_type:(fields_value (required Tick_type.val_type))
          ~use_rth:(fields_value (required bool)))
      (fun contract bar_size tick_type use_rth ->
         { contract; bar_size; tick_type; use_rth }))
end
