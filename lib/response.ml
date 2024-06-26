open Core
open Tws_prot


(* +-----------------------------------------------------------------------+
   | Connection and server                                                 |
   +-----------------------------------------------------------------------+ *)

module Tws_error = struct
  type t =
    { error_code : int
    ; error_msg : string
    } [@@deriving sexp, fields, eq]

  let create = Fields.create

  let ( = ) t1 t2 = equal t1 t2

  let decoder =
    Decoder.create ~name:"Response.Tws_error"
      Decoder.Spec.(
        Fields.fold
          ~init:(step Fn.id)
          ~error_code:(fields_value (required int))
          ~error_msg:(fields_value (required string)))
      (fun error_code error_msg  -> { error_code; error_msg })

  let encoder = Only_in_test.of_thunk (fun () ->
    Encoder.create ~name:"Response.Tws_error"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~error_code:(fields_value (required int))
            ~error_msg:(fields_value (required string)))
          (fun { error_code; error_msg } ->
             `Args $ error_code $ error_msg)))

  let to_string_hum t =
    let module F = Format in
    let pp ppf t = F.fprintf ppf "%i - %s" t.error_code t.error_msg in
    F.fprintf F.str_formatter "@[%a@]" pp t;
    F.flush_str_formatter ()

  let to_error t = Error.of_string (to_string_hum t)
  let to_exn t = Error.to_exn (to_error t)
  let raise t = Error.raise (to_error t)
end

module Server_time = struct
  type t = Time_float_unix.t [@@deriving sexp]

  let create ~time = time
  let equal t1 t2 = Time_float_unix.equal t1 t2
  let ( = ) t1 t2 = equal t1 t2

  let decoder =
    Decoder.create ~name:"Response.Server_time"
      Decoder.Spec.(value (required int64) ~name:"time")
      (fun long_int ->
         Int64.to_float long_int
         |> Unix.localtime
         |> Time_float_unix.of_tm ~zone:(Lazy.force Time_float_unix.Zone.local))

  let encoder = Only_in_test.of_thunk (fun () ->
    Encoder.create ~name:"Response.Server_time"
      Encoder.Spec.(lift (value (required int64)) (fun tm ->
        Time_float_unix.to_span_since_epoch tm
        |> Time_float_unix.Span.to_proportional_float
        |> Int64.of_float)))
end

(* +-----------------------------------------------------------------------+
   | Market data                                                           |
   +-----------------------------------------------------------------------+ *)

module Tick_price = struct
  module Type = struct
    type t =
      | Bid
      | Ask
      | Last
      | High
      | Low
      | Close
      | Open
    [@@deriving sexp, eq]

    let tws_of_t = function
      | Bid -> "1"
      | Ask -> "2"
      | Last -> "4"
      | High -> "6"
      | Low -> "7"
      | Close -> "9"
      | Open -> "14"

    let t_of_tws = function
      | "1" -> Bid
      | "2" -> Ask
      | "4" -> Last
      | "6" -> High
      | "7" -> Low
      | "9" -> Close
      | "14" -> Open
      | s -> invalid_argf "Type.t_of_tws: %S" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t =
    { tick_type : Type.t
    ; price : Price.t
    ; size : Volume.t
    ; can_auto_execute : bool option
    } [@@deriving sexp, fields]

  let create = Fields.create

  let price t = if Price.(t.price = neg one) then Price.nan else t.price

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~tick_type:(use Type.equal)
      ~price:(use Price.(=.))
      ~size:(use Volume.(=))
      ~can_auto_execute:(use (Option.equal Bool.(=)))

  let decoder =
    Decoder.create ~name:"Response.Tick_price"
      Decoder.Spec.(
        Fields.fold
          ~init:(step Fn.id)
          ~tick_type:(fields_value (required Type.val_type))
          ~price:(fields_value (optional_with_default ~default:Price.zero Price.val_type))
          ~size:(fields_value (required Volume.val_type))
          ~can_auto_execute:(fields_value (optional bool ~none_on_default:"-1")))
      (fun tick_type price size can_auto_execute ->
         { tick_type; price; size; can_auto_execute })

  let encoder = Only_in_test.of_thunk (fun () ->
    Encoder.create ~name:"Response.Tick_price"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~tick_type:(fields_value (required Type.val_type))
            ~price:(fields_value (required Price.val_type))
            ~size:(fields_value (required Volume.val_type))
            ~can_auto_execute:(fields_value (optional bool ~default_on_none:"-1")))
          (fun t -> `Args $ t.tick_type $ t.price $ t.size $ t.can_auto_execute)))

  let pp ppf t =
    Format.fprintf ppf "type=%s price=%4.2f size=%d %s"
      (Type.sexp_of_t t.tick_type |> Sexp.to_string_hum)
      (t.price :> float)
      (t.size :> int)
      (Option.value_map t.can_auto_execute ~default:"n/a" ~f:(function
         | true  -> "can_auto_execute"
         | false -> "no_auto_execute"))
end

module Tick_size = struct
  module Type = struct
    type t =
      | Bid
      | Ask
      | Last
      | Volume
    [@@deriving sexp, eq]

    let tws_of_t = function
      | Bid -> "0"
      | Ask -> "3"
      | Last -> "5"
      | Volume -> "8"

    let t_of_tws = function
      | "0" -> Bid
      | "3" -> Ask
      | "5" -> Last
      | "8" -> Volume
      | s -> invalid_argf "Type.t_of_tws: %S" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t =
    { tick_type : Type.t
    ; size : Volume.t
    } [@@deriving sexp, fields, eq]

  let create = Fields.create

  let ( = ) t1 t2 = equal t1 t2

  let decoder =
    Decoder.create ~name:"Response.Tick_size"
      Decoder.Spec.(
        Fields.fold
          ~init:(step Fn.id)
          ~tick_type:(fields_value (required Type.val_type))
          ~size:(fields_value (required Volume.val_type)))
      (fun tick_type size -> { tick_type; size })

  let encoder = Only_in_test.of_thunk (fun () ->
    Encoder.create ~name:"Response.Tick_size"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~tick_type:(fields_value (required Type.val_type))
            ~size:(fields_value (required Volume.val_type)))
          (fun t -> `Args $ t.tick_type $ t.size)))

  let pp ppf t =
    Format.fprintf ppf "type=%s size=%i"
      (Type.sexp_of_t t.tick_type |> Sexp.to_string_hum)
      (t.size :> int)
end

module Tick_option = struct
  module Type = struct
    type t =
      | Bid
      | Ask
      | Last
      | Model
      | Custom
    [@@deriving sexp, eq]

    let tws_of_t = function
      | Bid -> "10"
      | Ask -> "11"
      | Last -> "12"
      | Model -> "13"
      | Custom -> "53"

    let t_of_tws = function
      | "10" -> Bid
      | "11" -> Ask
      | "12" -> Last
      | "13" -> Model
      | "53" -> Custom
      | s -> invalid_argf "Type.t_of_tws: %S" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t =
    { tick_type : Type.t
    ; implied_vol : float
    ; delta : float
    ; option_price : Price.t
    ; pv_dividend : float
    ; gamma : float
    ; vega : float
    ; theta : float
    ; under_price : Price.t
    } [@@deriving sexp, fields]

  let create = Fields.create

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~tick_type:(use Type.equal)
      ~implied_vol:(use Float.(=.))
      ~delta:(use Float.(=.))
      ~option_price:(use Price.(=.))
      ~pv_dividend:(use Float.(=.))
      ~gamma:(use Float.(=.))
      ~vega:(use Float.(=.))
      ~theta:(use Float.(=.))
      ~under_price:(use Price.(=.))

  let decoder =
    Decoder.create ~name:"Response.Tick_option"
      Decoder.Spec.(
        Fields.fold
          ~init:(step Fn.id)
          ~tick_type:(fields_value (required Type.val_type))
          ~implied_vol:(fields_value (required float))
          ~delta:(fields_value (required float))
          ~option_price:(fields_value (required Price.val_type))
          ~pv_dividend:(fields_value (required float))
          ~gamma:(fields_value (required float))
          ~vega:(fields_value (required float))
          ~theta:(fields_value (required float))
          ~under_price:(fields_value (required Price.val_type)))
      (fun tick_type implied_vol delta option_price pv_dividend gamma vega theta under_price ->
         { tick_type;
           implied_vol = if Float.(implied_vol < 0.) then Float.nan else implied_vol;
           delta = if Float.(abs delta > 1.) then Float.nan else delta;
           option_price = if Price.(option_price < zero) then Price.nan else option_price;
           pv_dividend = if Float.(pv_dividend < 0.) then Float.nan else pv_dividend;
           gamma = if Float.(abs gamma > 1.) then Float.nan else gamma;
           vega = if Float.(abs vega > 1.) then Float.nan else vega;
           theta = if Float.(abs theta > 1.) then Float.nan else theta;
           under_price = if Price.(under_price < zero) then Price.nan else under_price;
         })

  let encoder = Only_in_test.of_thunk (fun () ->
    Encoder.create ~name:"Response.Tick_option"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~tick_type:(fields_value (required Type.val_type))
            ~implied_vol:(fields_value (required float))
            ~delta:(fields_value (required float))
            ~option_price:(fields_value (required Price.val_type))
            ~pv_dividend:(fields_value (required float))
            ~gamma:(fields_value (required float))
            ~vega:(fields_value (required float))
            ~theta:(fields_value (required float))
            ~under_price:(fields_value (required Price.val_type)))
          (fun t ->
             `Args
             $ t.tick_type
             $ (if Float.is_nan t.implied_vol then -1. else t.implied_vol)
             $ (if Float.is_nan t.delta then -2. else t.delta)
             $ (if Price.is_nan t.option_price then Price.(neg one) else t.option_price)
             $ (if Float.is_nan t.pv_dividend then -1. else t.pv_dividend)
             $ (if Float.is_nan t.gamma then -2. else t.gamma)
             $ (if Float.is_nan t.vega then -2. else t.vega)
             $ (if Float.is_nan t.theta then -2. else t.theta)
             $ (if Price.is_nan t.under_price then Price.(neg one) else t.under_price))))

  let pp ppf t =
    let float_to_string x = sprintf "%4.2f" x in
    let price_to_string x = float_to_string (Price.to_float x) in
    Format.fprintf ppf
      "type=%s vol=%s delta=%s gamma=%s vega=%s theta=%s \
       opt_price=%s pv_dividend=%s und_price=%s"
      (Type.sexp_of_t t.tick_type |> Sexp.to_string_hum)
      (if Float.is_nan t.implied_vol then "n/a" else float_to_string t.implied_vol)
      (if Float.is_nan t.delta then "n/a" else float_to_string t.delta)
      (if Float.is_nan t.gamma then "n/a" else float_to_string t.gamma)
      (if Float.is_nan t.vega then "n/a" else float_to_string t.vega)
      (if Float.is_nan t.theta then "n/a" else float_to_string t.theta)
      (if Price.is_nan t.option_price then "n/a" else price_to_string t.option_price)
      (if Float.is_nan t.pv_dividend then "n/a" else float_to_string t.pv_dividend)
      (if Price.is_nan t.under_price then "n/a" else price_to_string t.under_price)
end

module Tick_string = struct
  module Type = struct
    type t =
      | Bid_size
      | Bid_price
      | Ask_price
      | Ask_size
      | Last_price
      | Last_size
      | High_price
      | Low_price
      | Volume
      | Close_price
      | Bid_option
      | Ask_option
      | Last_option
      | Model_option
      | Open_price
      | Low_13_week
      | High_13_week
      | Low_26_week
      | High_26_week
      | Low_52_week
      | High_52_week
      | Avg_volume
      | Open_interest
      | Historical_volatility
      | Implied_volatility
      | Option_bid_exch
      | Option_ask_exch
      | Call_open_interest
      | Put_open_interest
      | Call_volume
      | Put_volume
      | Index_future_premium
      | Bid_exch
      | Ask_exch
      | Auction_volume
      | Auction_price
      | Auction_imbalance
      | Mark_price
      | Bid_efp
      | Ask_efp
      | Last_efp
      | Open_efp
      | High_efp
      | Low_efp
      | Close_efp
      | Last_timestamp
      | Shortable
      | Fundamental_ratios
      | Realtime_volume
      | Halted
      | Bid_yield
      | Ask_yield
      | Last_yield
      | Cust_option_comp
    [@@deriving sexp, eq]

    let tws_of_t = function
      | Bid_size -> "0"
      | Bid_price -> "1"
      | Ask_price -> "2"
      | Ask_size -> "3"
      | Last_price -> "4"
      | Last_size -> "5"
      | High_price -> "6"
      | Low_price -> "7"
      | Volume -> "8"
      | Close_price -> "9"
      | Bid_option -> "10"
      | Ask_option -> "11"
      | Last_option -> "12"
      | Model_option -> "13"
      | Open_price -> "14"
      | Low_13_week -> "15"
      | High_13_week -> "16"
      | Low_26_week -> "17"
      | High_26_week -> "18"
      | Low_52_week -> "19"
      | High_52_week -> "20"
      | Avg_volume -> "21"
      | Open_interest -> "22"
      | Historical_volatility -> "23"
      | Implied_volatility -> "24"
      | Option_bid_exch -> "25"
      | Option_ask_exch -> "26"
      | Call_open_interest -> "27"
      | Put_open_interest -> "28"
      | Call_volume -> "29"
      | Put_volume -> "30"
      | Index_future_premium -> "31"
      | Bid_exch -> "32"
      | Ask_exch -> "33"
      | Auction_volume -> "34"
      | Auction_price -> "35"
      | Auction_imbalance -> "36"
      | Mark_price -> "37"
      | Bid_efp -> "38"
      | Ask_efp -> "39"
      | Last_efp -> "40"
      | Open_efp -> "41"
      | High_efp -> "42"
      | Low_efp -> "43"
      | Close_efp -> "44"
      | Last_timestamp -> "45"
      | Shortable -> "46"
      | Fundamental_ratios -> "47"
      | Realtime_volume -> "48"
      | Halted -> "49"
      | Bid_yield -> "50"
      | Ask_yield -> "51"
      | Last_yield -> "52"
      | Cust_option_comp -> "53"

    let t_of_tws = function
      | "0" -> Bid_size
      | "1" -> Bid_price
      | "2" -> Ask_price
      | "3" -> Ask_size
      | "4" -> Last_price
      | "5" -> Last_size
      | "6" -> High_price
      | "7" -> Low_price
      | "8" -> Volume
      | "9"  -> Close_price
      | "10" -> Bid_option
      | "11" -> Ask_option
      | "12" -> Last_option
      | "13" -> Model_option
      | "14" -> Open_price
      | "15" -> Low_13_week
      | "16" -> High_13_week
      | "17" -> Low_26_week
      | "18" -> High_26_week
      | "19" -> Low_52_week
      | "20" -> High_52_week
      | "21" -> Avg_volume
      | "22" -> Open_interest
      | "23" -> Historical_volatility
      | "24" -> Implied_volatility
      | "25" -> Option_bid_exch
      | "26" -> Option_ask_exch
      | "27" -> Call_open_interest
      | "28" -> Put_open_interest
      | "29" -> Call_volume
      | "30" -> Put_volume
      | "31" -> Index_future_premium
      | "32" -> Bid_exch
      | "33" -> Ask_exch
      | "34" -> Auction_volume
      | "35" -> Auction_price
      | "36" -> Auction_imbalance
      | "37" -> Mark_price
      | "38" -> Bid_efp
      | "39" -> Ask_efp
      | "40" -> Last_efp
      | "41" -> Open_efp
      | "42" -> High_efp
      | "43" -> Low_efp
      | "44" -> Close_efp
      | "45" -> Last_timestamp
      | "46" -> Shortable
      | "47" -> Fundamental_ratios
      | "48" -> Realtime_volume
      | "49" -> Halted
      | "50" -> Bid_yield
      | "51" -> Ask_yield
      | "52" -> Last_yield
      | "53" -> Cust_option_comp
      | s  -> invalid_argf "Type.t_of_tws: %s" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t =
    { tick_type : Type.t
    ; value : string
    } [@@deriving fields, sexp]

  let create = Fields.create

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~tick_type:(use Type.equal)
      ~value:(use String.(=))

  let decoder =
    Decoder.create ~name:"Response.Tick_string"
      Decoder.Spec.(
        Fields.fold
          ~init:(step Fn.id)
          ~tick_type:(fields_value (required Type.val_type))
          ~value:(fields_value (required string)))
      (fun tick_type value -> { tick_type; value })

  let encoder = Only_in_test.of_thunk (fun () ->
    Encoder.create ~name:"Response.Tick_string"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~tick_type:(fields_value (required Type.val_type))
            ~value:(fields_value (required string)))
          (fun t -> `Args $ t.tick_type $ t.value)))

  let pp ppf t =
    Format.fprintf ppf "type=%s value=%s"
      (Type.sexp_of_t t.tick_type |> Sexp.to_string_hum)
      (match t.tick_type with
       | Type.Last_timestamp ->
         Float.of_string t.value
         |> Unix.localtime
         |> Time_float_unix.of_tm ~zone:(Lazy.force Time_float_unix.Zone.local)
         |> Time_float_unix.to_string_trimmed ~zone:(Lazy.force Time_float_unix.Zone.local)
       | _ -> t.value)
end

(* +-----------------------------------------------------------------------+
   | Orders                                                                |
   +-----------------------------------------------------------------------+ *)

module Order_status = struct
  module State = struct
    type t =
      [ `Pending_submit
      | `Pending_cancel
      | `Pre_submitted
      | `Submitted
      | `Cancelled
      | `Filled
      | `Inactive
      ] [@@deriving sexp, eq]

    let tws_of_t = function
      | `Pending_submit -> "PendingSubmit"
      | `Pending_cancel -> "PendingCancel"
      | `Pre_submitted -> "PreSubmitted"
      | `Submitted -> "Submitted"
      | `Cancelled -> "Cancelled"
      | `Filled -> "Filled"
      | `Inactive -> "Inactive"

    let t_of_tws = function
      | "PendingSubmit" -> `Pending_submit
      | "PendingCancel" -> `Pending_cancel
      | "PreSubmitted" -> `Pre_submitted
      | "Submitted" -> `Submitted
      | "Cancelled" -> `Cancelled
      | "Filled" -> `Filled
      | "Inactive" -> `Inactive
      | s -> invalid_argf "State.t_of_tws: %S" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t =
    { state : State.t
    ; filled : Volume.t
    ; remaining : Volume.t
    ; avg_fill_price : Price.t
    ; permanent_id : int
    ; parent_id : Order_id.t
    ; last_fill_price : Price.t
    ; client_id : Client_id.t
    ; why_held : string option
    } [@@deriving sexp, fields]

  let create = Fields.create

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~state:(use State.equal)
      ~filled:(use Volume.equal)
      ~remaining:(use Volume.equal)
      ~avg_fill_price:(use Price.(=.))
      ~permanent_id:(use (=))
      ~parent_id:(use Order_id.(=))
      ~last_fill_price:(use Price.(=.))
      ~client_id:(use Client_id.(=))
      ~why_held:(use (Option.equal String.(=)))

  let decoder =
    Decoder.create ~name:"Response.Order_status"
      Decoder.Spec.(
        Fields.fold
          ~init:(empty ())
          ~state:(fields_value (required State.val_type))
          ~filled:(fields_value (required Volume.val_type))
          ~remaining:(fields_value (required Volume.val_type))
          ~avg_fill_price:(fields_value (required Price.val_type))
          ~permanent_id:(fields_value (required int))
          ~parent_id:(fields_value (required Order_id.val_type))
          ~last_fill_price:(fields_value (required Price.val_type))
          ~client_id:(fields_value (required Client_id.val_type))
          ~why_held:(fields_value (optional string)))
      (fun state filled remaining avg_fill_price permanent_id parent_id
        last_fill_price client_id why_held ->
        { state;
          filled;
          remaining;
          avg_fill_price;
          permanent_id;
          parent_id;
          last_fill_price;
          client_id;
          why_held;
        })

  let encoder = Only_in_test.of_thunk (fun () ->
    Encoder.create ~name:"Response.Order_status"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~state:(fields_value (required State.val_type))
            ~filled:(fields_value (required Volume.val_type))
            ~remaining:(fields_value (required Volume.val_type))
            ~avg_fill_price:(fields_value (required Price.val_type))
            ~permanent_id:(fields_value (required int))
            ~parent_id:(fields_value (required Order_id.val_type))
            ~last_fill_price:(fields_value (required Price.val_type))
            ~client_id:(fields_value (required Client_id.val_type))
            ~why_held:(fields_value (optional string)))
          (fun t ->
             `Args
             $ t.state
             $ t.filled
             $ t.remaining
             $ t.avg_fill_price
             $ t.permanent_id
             $ t.parent_id
             $ t.last_fill_price
             $ t.client_id
             $ t.why_held)))
end

(* +-----------------------------------------------------------------------+
   | Account and Portfolio                                                 |
   +-----------------------------------------------------------------------+ *)

module Account_update = struct
  type t =
    { key : string
    ; value : string
    ; currency : string option
    ; account_code : Account_code.t
    } [@@deriving sexp, fields]

  let create = Fields.create

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~key:(use String.(=))
      ~value:(use String.(=))
      ~currency:(use (Option.equal String.(=)))
      ~account_code:(use Account_code.equal)

  let decoder =
    Decoder.create ~name:"Response.Account_update"
      Decoder.Spec.(
        Fields.fold
          ~init:(empty ())
          ~key:(fields_value (required string))
          ~value:(fields_value (required string))
          ~currency:(fields_value (optional string))
          ~account_code:(fields_value (required Account_code.val_type)))
      (fun key value currency account_code ->
         { key; value; currency; account_code })

  let encoder = Only_in_test.of_thunk (fun () ->
    Encoder.create ~name:"Response.Account_update"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~key:(fields_value (required string))
            ~value:(fields_value (required string))
            ~currency:(fields_value (optional string))
            ~account_code:(fields_value (required Account_code.val_type)))
          (fun t -> `Args $ t.key $ t.value $ t.currency $ t.account_code)))
end

module Position = struct
  type t =
    { contract : Raw_contract.t
    ; size : Volume.t
    ; market_price : Price.t
    ; market_value : Price.t
    ; average_cost : Price.t
    ; unrealized_pnl : Price.t
    ; realized_pnl : Price.t
    ; account_code : Account_code.t
    } [@@deriving sexp, fields]

  let create ~contract ~size ~market_price ~market_value ~average_cost
      ~unrealized_pnl ~realized_pnl ~account_code =
    { contract = Contract.to_raw contract
    ; size
    ; market_price
    ; market_value
    ; average_cost
    ; unrealized_pnl
    ; realized_pnl
    ; account_code
    }

  let contract t = Contract.of_raw t.contract

  let total_pnl t = Price.(t.unrealized_pnl + t.realized_pnl)

  let return t =
    let size = Volume.to_float t.size in
    let market_value = (t.market_value :> float) in
    let average_cost = (t.average_cost :> float) in
    Float.(match robust_sign size with
      | Sign.Zero -> zero
      | Sign.Pos  ->     (market_value / (average_cost * size) - one)
      | Sign.Neg  -> neg (market_value / (average_cost * size) - one)
    )
  ;;

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~contract:(use Raw_contract.(=))
      ~size:(use Volume.(=))
      ~market_price:(use Price.(=.))
      ~market_value:(use Price.(=.))
      ~average_cost:(use Price.(=.))
      ~unrealized_pnl:(use Price.(=.))
      ~realized_pnl:(use Price.(=.))
      ~account_code:(use Account_code.(=))

  let decoder =
    let contract_spec = Raw_contract.Decoder_specs.position_response () in
    Decoder.create ~name:"Response.Position"
      Decoder.Spec.(
        Fields.fold
          ~init:(empty ())
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~size:(fields_value (required Volume.val_type))
          ~market_price:(fields_value (required Price.val_type))
          ~market_value:(fields_value (required Price.val_type))
          ~average_cost:(fields_value (required Price.val_type))
          ~unrealized_pnl:(fields_value (required Price.val_type))
          ~realized_pnl:(fields_value (required Price.val_type))
          ~account_code:(fields_value (required Account_code.val_type)))
      (fun contract size market_price market_value average_cost
        unrealized_pnl realized_pnl account_code ->
        { contract;
          size;
          market_price;
          market_value;
          average_cost;
          unrealized_pnl;
          realized_pnl;
          account_code;
        })

  let encoder = Only_in_test.of_thunk (fun () ->
    let contract_spec =
      Raw_contract.Encoder_specs.position_response ()
    in
    Encoder.create ~name:"Response.Position"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~size:(fields_value (required Volume.val_type))
            ~market_price:(fields_value (required Price.val_type))
            ~market_value:(fields_value (required Price.val_type))
            ~average_cost:(fields_value (required Price.val_type))
            ~unrealized_pnl:(fields_value (required Price.val_type))
            ~realized_pnl:(fields_value (required Price.val_type))
            ~account_code:(fields_value (required Account_code.val_type)))
          (fun t ->
             `Args
             $ t.contract
             $ t.size
             $ t.market_price
             $ t.market_value
             $ t.average_cost
             $ t.unrealized_pnl
             $ t.realized_pnl
             $ t.account_code)))
end

(* +-----------------------------------------------------------------------+
   | Contract details                                                      |
   +-----------------------------------------------------------------------+ *)

module Contract_data = struct
  type t =
    { contract : Raw_contract.t
    ; market_name : string
    ; trading_class : string
    ; min_tick : float
    ; order_types : string list
    ; valid_exchanges : Exchange.t list
    ; price_magnifier : int
    ; underlying_id : int
    ; long_name : string
    ; contract_month : string
    ; industry : string
    ; category : string
    ; subcategory : string
    ; time_zone : Time_float_unix.Zone.t option
    ; trading_hours : Trading_times.t list
    ; liquid_hours : Trading_times.t list
    } [@@deriving sexp, fields]

  let create ~contract ~market_name ~trading_class ~min_tick ~order_types
      ~valid_exchanges ~price_magnifier ~underlying_id ~long_name ~contract_month
      ~industry ~category ~subcategory ~time_zone ~trading_hours ~liquid_hours =
    { contract = Contract.to_raw contract
    ; market_name
    ; trading_class
    ; min_tick
    ; order_types
    ; valid_exchanges
    ; price_magnifier
    ; underlying_id
    ; long_name
    ; contract_month
    ; industry
    ; category
    ; subcategory
    ; time_zone = Some time_zone
    ; trading_hours
    ; liquid_hours
    }

  let contract t = Contract.of_raw t.contract

  let regular_trading_times  t = List.hd_exn t.liquid_hours
  let extended_trading_times t = List.hd_exn t.trading_hours

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~contract:(use Raw_contract.(=))
      ~market_name:(use String.(=))
      ~trading_class:(use String.(=))
      ~min_tick:(use Float.(=.))
      ~order_types:(use (List.equal String.(=)))
      ~valid_exchanges:(use (List.equal Exchange.equal))
      ~price_magnifier:(use (=))
      ~underlying_id:(use (=))
      ~long_name:(use String.(=))
      ~contract_month:(use String.(=))
      ~industry:(use String.(=))
      ~category:(use String.(=))
      ~subcategory:(use String.(=))
      ~time_zone:(use (Option.equal Time_float_unix.Zone.(=)))
      ~trading_hours:(use (List.equal Trading_times.equal))
      ~liquid_hours:(use (List.equal Trading_times.equal))

  let decoder =
    let field_name field = Fieldslib.Field.name field in
    Decoder.create ~name:"Response.Contract_data"
      Decoder.Spec.(
        value (required Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.symbol)
        ++ value (required string)
          ~name:(field_name Raw_contract.Fields.sec_type)
        ++ value (optional date)
          ~name:(field_name Raw_contract.Fields.expiry)
        ++ value (optional Price.val_type)
          ~name:(field_name Raw_contract.Fields.strike)
        ++ value (optional Option_right.val_type)
          ~name:(field_name Raw_contract.Fields.right)
        ++ value (required Exchange.val_type)
          ~name:(field_name Raw_contract.Fields.exchange)
        ++ value (required Currency.val_type)
          ~name:(field_name Raw_contract.Fields.currency)
        ++ value (optional Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.local_symbol)
        ++ value (required string)
          ~name:(field_name Fields.market_name)
        ++ value (required string)
          ~name:(field_name Fields.trading_class)
        ++ value (optional Contract_id.val_type)
          ~name:(field_name Raw_contract.Fields.con_id)
        ++ value (required float)
          ~name:(field_name Fields.min_tick)
        ++ value (optional int)
          ~name:(field_name Raw_contract.Fields.multiplier)
        ++ value (sequence string)
          ~name:(field_name Fields.order_types)
        ++ value (sequence Exchange.val_type)
          ~name:(field_name Fields.valid_exchanges)
        ++ value (required int)
          ~name:(field_name Fields.price_magnifier)
        ++ value (required int)
          ~name:(field_name Fields.underlying_id)
        ++ value (required string)
          ~name:(field_name Fields.long_name)
        ++ value (optional Exchange.val_type)
          ~name:(field_name Raw_contract.Fields.prim_exch)
        ++ value (required string)
          ~name:(field_name Fields.contract_month)
        ++ value (required string)
          ~name:(field_name Fields.industry)
        ++ value (required string)
          ~name:(field_name Fields.category)
        ++ value (required string)
          ~name:(field_name Fields.subcategory)
        ++ value (optional zone)
          ~name:(field_name Fields.time_zone)
        ++ value (sequence Trading_times.val_type ~sep:';')
          ~name:(field_name Fields.trading_hours)
        ++ value (sequence Trading_times.val_type ~sep:';')
          ~name:(field_name Fields.liquid_hours))
      (fun symbol sec_type expiry strike right exchange currency local_symbol
        market_name trading_class con_id min_tick multiplier order_types
        valid_exchanges price_magnifier underlying_id long_name prim_exch
        contract_month industry category subcategory time_zone trading_hours
        liquid_hours ->
        let contract =
          { Raw_contract.
            con_id
          ; symbol
          ; sec_type
          ; expiry
          ; strike
          ; right
          ; multiplier
          ; exchange
          ; prim_exch
          ; currency
          ; local_symbol
          ; include_expired = false
          ; sec_id_type = None
          ; sec_id = None
          ; combo_legs = 0
          }
        in
        { contract
        ; market_name
        ; trading_class
        ; min_tick
        ; order_types
        ; valid_exchanges
        ; price_magnifier
        ; underlying_id
        ; long_name
        ; contract_month
        ; industry
        ; category
        ; subcategory
        ; time_zone
        ; trading_hours
        ; liquid_hours
        })

  let encoder = Only_in_test.of_thunk (fun () ->
    Encoder.create ~name:"Response.Contract_data"
      Encoder.Spec.(
        lift (empty ()
              (* symbol *)
              ++ value (required Symbol.val_type)
              (* contract type *)
              ++ value (required string)
              (* expiry *)
              ++ value (optional date)
              (* strike *)
              ++ value (optional Price.val_type)
              (* option right *)
              ++ value (optional Option_right.val_type)
              (* exchange *)
              ++ value (required Exchange.val_type)
              (* currency *)
              ++ value (required Currency.val_type)
              (* local symbol *)
              ++ value (optional Symbol.val_type)
              (* market name *)
              ++ value (required string)
              (* trading class *)
              ++ value (required string)
              (* contract id *)
              ++ value (optional Contract_id.val_type)
              (* min tick *)
              ++ value (required float)
              (* multiplier *)
              ++ value (optional int)
              (* order types *)
              ++ value (sequence string)
              (* valid exchanges *)
              ++ value (sequence Exchange.val_type)
              (* price magnifier *)
              ++ value (required int)
              (* underlying id *)
              ++ value (required int)
              (* long name *)
              ++ value (required string)
              (* listing exchange *)
              ++ value (optional Exchange.val_type)
              (* contract month *)
              ++ value (required string)
              (* industry *)
              ++ value (required string)
              (* category *)
              ++ value (required string)
              (* subcategory *)
              ++ value (required string)
              (* time_zone *)
              ++ value (optional zone)
              (* trading hours *)
              ++ value (sequence Trading_times.val_type ~sep:';')
              (* liquid hours *)
              ++ value (sequence Trading_times.val_type ~sep:';'))
          (fun t ->
             `Args
             $ t.contract.Raw_contract.symbol
             $ t.contract.Raw_contract.sec_type
             $ t.contract.Raw_contract.expiry
             $ t.contract.Raw_contract.strike
             $ t.contract.Raw_contract.right
             $ t.contract.Raw_contract.exchange
             $ t.contract.Raw_contract.currency
             $ t.contract.Raw_contract.local_symbol
             $ t.market_name
             $ t.trading_class
             $ t.contract.Raw_contract.con_id
             $ t.min_tick
             $ t.contract.Raw_contract.multiplier
             $ t.order_types
             $ t.valid_exchanges
             $ t.price_magnifier
             $ t.underlying_id
             $ t.long_name
             $ t.contract.Raw_contract.prim_exch
             $ t.contract_month
             $ t.industry
             $ t.category
             $ t.subcategory
             $ t.time_zone
             $ t.trading_hours
             $ t.liquid_hours
          )
      ))
end

(* +-----------------------------------------------------------------------+
   | Executions                                                            |
   +-----------------------------------------------------------------------+ *)

module Execution = struct
  module Side = struct
    module T = struct
      type t =
        [ `bought
        | `sold
        ] [@@deriving sexp, eq]
    end
    include T
    include Sexpable.To_stringable (T)

    let tws_of_t = function
      | `bought -> "BOT"
      | `sold    -> "SLD"

    let t_of_tws = function
      | "BOT" -> `bought
      | "SLD" -> `sold
      | s -> invalid_argf "Side.t_of_tws: %S" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t =
    { order_id : Order_id.t
    ; contract : Raw_contract.t
    ; exec_id : Execution_id.t
    ; time : Time_float_unix.t
    ; account_code : Account_code.t
    ; exchange : Exchange.t
    ; side : Side.t
    ; volume : Volume.t
    ; price : Price.t
    ; permanent_id : int
    ; client_id : Client_id.t
    ; liquidation : int
    ; cumulative_volume : Volume.t
    ; average_price : Price.t
    ; order_ref : string option
    } [@@deriving fields, sexp]

  let create ~order_id ~contract ~exec_id ~time ~account_code ~exchange ~side
      ~volume ~price ~permanent_id ~client_id ~liquidation ~cumulative_volume
      ~average_price ~order_ref =
    { order_id
    ; contract = Contract.to_raw contract
    ; exec_id
    ; time
    ; account_code
    ; exchange
    ; side
    ; volume
    ; price
    ; permanent_id
    ; client_id
    ; liquidation
    ; cumulative_volume
    ; average_price
    ; order_ref
    }

  let contract t = Contract.of_raw t.contract

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~order_id:(use Order_id.(=))
      ~contract:(use Raw_contract.(=))
      ~exec_id:(use Execution_id.(=))
      ~time:(use Time_float_unix.(=))
      ~account_code:(use Account_code.(=))
      ~exchange:(use Exchange.equal)
      ~side:(use Side.equal)
      ~volume:(use Volume.(=))
      ~price:(use Price.(=.))
      ~permanent_id:(use (=))
      ~client_id:(use Client_id.(=))
      ~liquidation:(use (=))
      ~cumulative_volume:(use Volume.(=))
      ~average_price:(use Price.(=.))
      ~order_ref:(use (Option.equal String.(=)))

  let decoder =
    let contract_spec = Raw_contract.Decoder_specs.execution_response () in
    Decoder.create ~name:"Response.Execution"
      Decoder.Spec.(
        Fields.fold
          ~init:(empty ())
          ~order_id:(fields_value (required Order_id.val_type))
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~exec_id:(fields_value (required Execution_id.val_type))
          ~time:(fields_value (required time))
          ~account_code:(fields_value (required Account_code.val_type))
          ~exchange:(fields_value (required Exchange.val_type))
          ~side:(fields_value (required Side.val_type))
          ~volume:(fields_value (required Volume.val_type))
          ~price:(fields_value (required Price.val_type))
          ~permanent_id:(fields_value (required int))
          ~client_id:(fields_value (required Client_id.val_type))
          ~liquidation:(fields_value (required int))
          ~cumulative_volume:(fields_value (required Volume.val_type))
          ~average_price:(fields_value (required Price.val_type))
          ~order_ref:(fields_value (optional string)))
      (fun order_id contract exec_id time account_code exchange side
        volume price permanent_id client_id liquidation cumulative_volume
        average_price order_ref ->
        { order_id
        ; contract
        ; exec_id
        ; time
        ; account_code
        ; exchange
        ; side
        ; volume
        ; price
        ; permanent_id
        ; client_id
        ; liquidation
        ; cumulative_volume
        ; average_price
        ; order_ref
        })

  let encoder = Only_in_test.of_thunk (fun () ->
    let contract_spec = Raw_contract.Encoder_specs.execution_response () in
    Encoder.create ~name:"Response.Execution"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~order_id:(fields_value (required Order_id.val_type))
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~exec_id:(fields_value (required Execution_id.val_type))
            ~time:(fields_value (required time))
            ~account_code:(fields_value (required Account_code.val_type))
            ~exchange:(fields_value (required Exchange.val_type))
            ~side:(fields_value (required Side.val_type))
            ~volume:(fields_value (required Volume.val_type))
            ~price:(fields_value (required Price.val_type))
            ~permanent_id:(fields_value (required int))
            ~client_id:(fields_value (required Client_id.val_type))
            ~liquidation:(fields_value (required int))
            ~cumulative_volume:(fields_value (required Volume.val_type))
            ~average_price:(fields_value (required Price.val_type))
            ~order_ref:(fields_value (optional string)))
          (fun t ->
             `Args
             $ t.order_id
             $ t.contract
             $ t.exec_id
             $ t.time
             $ t.account_code
             $ t.exchange
             $ t.side
             $ t.volume
             $ t.price
             $ t.permanent_id
             $ t.client_id
             $ t.liquidation
             $ t.cumulative_volume
             $ t.average_price
             $ t.order_ref)))

  let pp ppf t =
    Format.fprintf ppf
      "Execution: exec_id=%s time=%s exchange=%s side=%s shares=%d price=%4.2f"
      (t.exec_id  |> Execution_id.to_string)
      (t.time     |> Time_float_unix.to_string_trimmed ~zone:(Lazy.force Time_float_unix.Zone.local))
      (t.exchange |> Exchange.to_string)
      (t.side     |> Side.to_string)
      (t.volume   :> int)
      (t.price    :> float)
end

module Commission = struct
  type t =
    { exec_id : Execution_id.t
    ; commission : Price.t
    ; currency : Currency.t
    ; realized_pnl : Price.t
    ; yield : float
    ; yield_redemption_date : int option
    } [@@deriving sexp, fields]

  let create = Fields.create

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~exec_id:(use Execution_id.equal)
      ~commission:(use Price.(=.))
      ~currency:(use Currency.equal)
      ~realized_pnl:(use Price.(=.))
      ~yield:(use Float.(=.))
      ~yield_redemption_date:(use (Option.equal (=)))

  let decoder =
    Decoder.create ~name:"Response.Commission"
      Decoder.Spec.(
        Fields.fold
          ~init:(empty ())
          ~exec_id:(fields_value (required Execution_id.val_type))
          ~commission:(fields_value (required Price.val_type))
          ~currency:(fields_value (required Currency.val_type))
          ~realized_pnl:(fields_value (required Price.val_type))
          ~yield:(fields_value (required float))
          ~yield_redemption_date:(fields_value (optional int)))
      (fun exec_id commission currency realized_pnl
        yield yield_redemption_date ->
        { exec_id
        ; commission
        ; currency
        ; realized_pnl
        ; yield
        ; yield_redemption_date
        })

  let encoder = Only_in_test.of_thunk (fun () ->
    Encoder.create ~name:"Response.Commission"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~exec_id:(fields_value (required Execution_id.val_type))
            ~commission:(fields_value (required Price.val_type))
            ~currency:(fields_value (required Currency.val_type))
            ~realized_pnl:(fields_value (required Price.val_type))
            ~yield:(fields_value (required float))
            ~yield_redemption_date:(fields_value (optional int)))
          (fun t ->
             `Args
             $ t.exec_id
             $ t.commission
             $ t.currency
             $ t.realized_pnl
             $ t.yield
             $ t.yield_redemption_date)))
end

(* +-----------------------------------------------------------------------+
   | Market depth                                                          |
   +-----------------------------------------------------------------------+ *)

module Book_update = struct
  module Operation = struct
    type t = Insert | Update | Delete [@@deriving sexp, eq]

    let tws_of_t = function
      | Insert -> "0"
      | Update -> "1"
      | Delete -> "2"

    let t_of_tws = function
      | "0" -> Insert
      | "1" -> Update
      | "2" -> Delete
      | s -> invalid_argf "Operation.t_of_tws: %S" s  ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  module Side = struct
    type t =
      | Ask
      | Bid
    [@@deriving sexp, eq]

    let tws_of_t = function
      | Ask -> "0"
      | Bid -> "1"

    let t_of_tws = function
      | "0" -> Ask
      | "1" -> Bid
      | s -> invalid_argf "Side.t_of_tws: %S" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t =
    { position : int
    ; operation : Operation.t
    ; side : Side.t
    ; price : Price.t
    ; size : Volume.t
    } [@@deriving sexp, fields]

  let create = Fields.create

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~position:(use (=))
      ~operation:(use Operation.equal)
      ~side:(use Side.equal)
      ~price:(use Price.(=.))
      ~size:(use Volume.(=))

  let decoder =
    Decoder.create ~name:"Response.Book_update"
      Decoder.Spec.(
        Fields.fold
          ~init:(empty ())
          ~position:(fields_value (required int))
          ~operation:(fields_value (required Operation.val_type))
          ~side:(fields_value (required Side.val_type))
          ~price:(fields_value (required Price.val_type))
          ~size:(fields_value (required Volume.val_type)))
      (fun position operation side price size ->
         { position; operation; side; price; size })

  let encoder = Only_in_test.of_thunk (fun () ->
    Encoder.create ~name:"Response.Book_update"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~position:(fields_value (required int))
            ~operation:(fields_value (required Operation.val_type))
            ~side:(fields_value (required Side.val_type))
            ~price:(fields_value (required Price.val_type))
            ~size:(fields_value (required Volume.val_type)))
          (fun t ->
             `Args $ t.position $ t.operation $ t.side $ t.price $ t.size)))
end

(* +-----------------------------------------------------------------------+
   | History                                                               |
   +-----------------------------------------------------------------------+ *)

module History = struct
  type t =
    { start : Time_float_unix.t
    ; stop : Time_float_unix.t
    ; num_bars : int
    ; bars : Bar.t list
    } [@@deriving sexp, fields]

  let create ~bars =
    let num_bars = List.length bars in
    if num_bars = 0 then
      { start = Time_float_unix.epoch
      ; stop = Time_float_unix.epoch
      ; num_bars
      ; bars = []
      }
    else
      { start = List.hd_exn bars |> Bar.stamp
      ; stop = List.last_exn bars |> Bar.stamp
      ; num_bars = List.length bars
      ; bars
      }

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~start:(use Time_float_unix.equal)
      ~stop:(use Time_float_unix.equal)
      ~num_bars:(use (=))
      ~bars:(use (List.for_all2_exn ~f:Bar.(=)))

  let decoder =
    Decoder.create ~name:"Response.History"
      Decoder.Spec.(
        Fields.fold
          ~init:(empty ())
          ~start:(fields_value (required string))
          ~stop:(fields_value (required string))
          ~num_bars:(fields_value (required int))
          ~bars:(fun specs -> Fn.const (specs ++ capture_remaining_message)))
      (fun _start _stop num_bars bars_msg ->
         let num_fields = 9 in
         let bars_msg = Queue.to_array bars_msg in
         let bars = List.map (List.range 0 num_bars) ~f:(fun i ->
           let bar_msg =
             Array.sub bars_msg ~pos:(num_fields * i) ~len:num_fields
             |> Queue.of_array
           in
           let bar_u = Decoder.create
               (Raw_bar.Historical_bar.decoder_spec ()) Fn.id
           in
           Decoder.run_exn bar_u bar_msg
           |> Bar.of_raw)
         in
         create ~bars
      )

  let encoder = Only_in_test.of_thunk (fun () ->
    Encoder.create ~name:"Response.History"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~start:(fields_value (required string))
            ~stop:(fields_value (required string))
            ~num_bars:(fields_value (required int))
            ~bars:(fields_value tws_data))
          (fun t ->
             let bar_p = Encoder.create (Raw_bar.Historical_bar.encoder_spec ())
             in
             let bars_msg =
               List.map t.bars ~f:(fun bar -> Encoder.run bar_p (Bar.to_raw bar))
               |> String.concat
             in
             `Args
             $ Time_float_unix.to_string t.start
             $ Time_float_unix.to_string t.stop
             $ t.num_bars
             $ bars_msg)))

  module Data_frame = struct
    type t =
      { stamps : Time_float_unix.t array
      ; op : float array
      ; hi : float array
      ; lo : float array
      ; cl : float array
      ; vo : int   array
      } [@@deriving sexp, fields]
  end

  let unpack_bars t =
    let stamps = Array.create ~len:t.num_bars Time_float_unix.epoch in
    let op = Array.create ~len:t.num_bars Float.nan in
    let hi = Array.create ~len:t.num_bars Float.nan in
    let lo = Array.create ~len:t.num_bars Float.nan in
    let cl = Array.create ~len:t.num_bars Float.nan in
    let vo = Array.create ~len:t.num_bars Int.zero  in
    List.iteri t.bars ~f:(fun i bar ->
      Array.set stamps i bar.Bar.stamp;
      Array.set op i (bar.Bar.op :> float);
      Array.set hi i (bar.Bar.hi :> float);
      Array.set lo i (bar.Bar.lo :> float);
      Array.set cl i (bar.Bar.cl :> float);
      Array.set vo i (bar.Bar.vo :> int);
    );
    { Data_frame.stamps; op; hi; lo; cl; vo; }

  let time_ohlc t =
    let ohlc = List.map t.bars ~f:(fun bar ->
      Bar.(stamp bar, (op bar, hi bar, lo bar, cl bar)))
    in
    (ohlc :> (Time_float_unix.t * (float * float * float * float)) list)

  type vwap_item =
    { stamp : Time_float_unix.t
    ; total_value : float
    ; total_volume : float
    }

  let time_vwap t =
    let price bar = Bar.wap bar |> Price.to_float in
    let volume bar = Bar.vo bar |> Volume.to_float in
    match List.hd t.bars with
    | None -> []
    | Some bar ->
      let item =
        { stamp = Bar.stamp bar
        ; total_value = Float.(price bar * volume bar)
        ; total_volume = volume bar }
      in
      List.fold t.bars ~init:(item, [item]) ~f:(fun (item, items) bar ->
        let item' =
          { stamp = Bar.stamp bar
          ; total_value = Float.(item.total_value + price bar * volume bar)
          ; total_volume = Float.(item.total_volume + volume bar) }
        in
        item', item' :: items)
      |> Tuple2.get2
      |> List.map ~f:(fun x -> x.stamp, Float.(x.total_value / x.total_volume))
end

(* +-----------------------------------------------------------------------+
   | Realtime bars                                                         |
   +-----------------------------------------------------------------------+ *)

module Realtime_bar = struct
  type t = Bar.t [@@deriving sexp]

  let create ~bar = bar
  let ( = ) = Bar.(=)

  let decoder = Decoder.create
      (Raw_bar.Realtime_bar.decoder_spec ())
      Bar.of_raw

  let encoder = Only_in_test.of_thunk (fun () ->
    Encoder.create
      Encoder.Spec.(
        lift (Raw_bar.Realtime_bar.encoder_spec ()) Bar.to_raw))
end
