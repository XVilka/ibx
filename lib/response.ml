(* File: response.ml

   IBX - OCaml implementation of the Interactive Brokers TWS API

   Copyright (C) 2013-  Oliver Gu
   email: gu.oliver@yahoo.com

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Core.Std
open Std_internal
open Tws_prot

let wrap_contract_spec contract_spec =
  Pickler.Spec.(
    wrap contract_spec
      (fun contract ->
        `Args
          $ contract.Raw_contract.contract_id
          $ contract.Raw_contract.symbol
          $ contract.Raw_contract.contract_type
          $ contract.Raw_contract.expiry
          $ contract.Raw_contract.strike
          $ contract.Raw_contract.option_right
          $ contract.Raw_contract.multiplier
          $ contract.Raw_contract.exchange
          $ contract.Raw_contract.listing_exchange
          $ contract.Raw_contract.currency
          $ contract.Raw_contract.local_symbol
          $ contract.Raw_contract.include_expired
          $ contract.Raw_contract.security_id_type
          $ contract.Raw_contract.security_id
          $ contract.Raw_contract.combo_legs))

let field_name field = Fieldslib.Field.name field

module Timestamp = struct
  include Time
  let tws_of_t tm = Time.format tm "%Y%m%d  %H:%M:%S"
  let t_of_tws s =
    let unescape = unstage (String.Escaping.unescape ~escape_char:' ') in
    Time.of_string (unescape s)
  let val_type = Val_type.create tws_of_t t_of_tws
end

(* +-----------------------------------------------------------------------+
   | Connection and server                                                 |
   +-----------------------------------------------------------------------+ *)

module Tws_error = struct
  type t =
    { error_code : int;
      error_msg : string;
    } with sexp, fields

  let create = Fields.create

  let ( = ) t1 t2 = (t1 = t2)

  let unpickler =
    Unpickler.create ~name:"Tws_error"
      Unpickler.Spec.(
        Fields.fold
          ~init:(step Fn.id)
          ~error_code:(fields_value (required int))
          ~error_msg:(fields_value (required string)))
      (fun error_code error_msg  -> { error_code; error_msg })

  let pickler = Only_in_test.of_thunk (fun () ->
    Pickler.create ~name:"Tws_error"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~error_code:(fields_value (required int))
            ~error_msg:(fields_value (required string)))
          (fun { error_code; error_msg } ->
            `Args $ error_code $ error_msg)))

  let pp ppf t = Format.fprintf ppf "%i - %s" t.error_code t.error_msg

  let to_string_hum t =
    let module F = Format in
    F.fprintf F.str_formatter "@[%a@]" pp t;
    F.flush_str_formatter ()
end

module Server_time = struct
  type t = Time.t with sexp

  let create ~time = time
  let ( = ) t1 t2 = (t1 = t2)

  let unpickler =
    Unpickler.create ~name:"Server_time"
      Unpickler.Spec.(value (required int64) ~name:"time")
      (fun long_int -> Time.of_float (Int64.to_float long_int))

  let pickler = Only_in_test.of_thunk (fun () ->
    Pickler.create ~name:"Server_time"
      Pickler.Spec.(wrap (value (required int64))
                      (fun tm -> Int64.of_float (Time.to_float tm))))
end

(* +-----------------------------------------------------------------------+
   | Market data                                                           |
   +-----------------------------------------------------------------------+ *)

module Tick_price = struct
  module Type = struct
    type t = Bid | Ask | Last | High | Low | Close with sexp

    let tws_of_t = function
      | Bid -> "1"
      | Ask -> "2"
      | Last -> "4"
      | High -> "6"
      | Low -> "7"
      | Close -> "9"

    let t_of_tws = function
      | "1" -> Bid
      | "2" -> Ask
      | "4" -> Last
      | "6" -> High
      | "7" -> Low
      | "9" -> Close
      | s -> invalid_argf "Type.t_of_tws: %S" s ()

    let val_type = Val_type.create tws_of_t t_of_tws
  end

  type t =
    { tick_type : Type.t;
      price : Price.t;
      size : int;
      can_auto_execute : bool option;
    } with sexp, fields

  let create = Fields.create

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~tick_type:(use (=))
      ~price:(use Price.(=.))
      ~size:(use (=))
      ~can_auto_execute:(use (Option.equal (=)))

  let unpickler =
    Unpickler.create ~name:"Tick_price"
      Unpickler.Spec.(
        Fields.fold
          ~init:(step Fn.id)
          ~tick_type:(fields_value (required Type.val_type))
          ~price:(fields_value (optional_with_default ~default:Price.zero Price.val_type))
          ~size:(fields_value (required int))
          ~can_auto_execute:(fields_value (optional bool ~none_on_default:"-1")))
      (fun tick_type price size can_auto_execute ->
        { tick_type; price; size; can_auto_execute })

  let pickler = Only_in_test.of_thunk (fun () ->
    Pickler.create ~name:"Tick_price"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~tick_type:(fields_value (required Type.val_type))
            ~price:(fields_value (required Price.val_type))
            ~size:(fields_value (required int))
            ~can_auto_execute:(fields_value (optional bool ~default_on_none:"-1")))
          (fun t -> `Args $ t.tick_type $ t.price $ t.size $ t.can_auto_execute)))

  let pp ppf t =
    Format.fprintf ppf "type=%s price=%4.2f size=%d %s"
      (Type.sexp_of_t t.tick_type |> Sexp.to_string_hum)
      (Price.to_float t.price)
      t.size
      (Option.value_map t.can_auto_execute ~default:"n/a" ~f:(function
        | true  -> "can_auto_execute"
        | false -> "no_auto_execute"))
end

module Tick_size = struct
  module Type = struct
    type t = Bid | Ask | Last | Volume with sexp

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
    { tick_type : Type.t;
      size : int;
    } with sexp, fields

  let create = Fields.create

  let ( = ) t1 t2 = (t1 = t2)

  let unpickler =
    Unpickler.create ~name:"Tick_size"
      Unpickler.Spec.(
        Fields.fold
          ~init:(step Fn.id)
          ~tick_type:(fields_value (required Type.val_type))
          ~size:(fields_value (required int)))
      (fun tick_type size -> { tick_type; size })

  let pickler = Only_in_test.of_thunk (fun () ->
    Pickler.create ~name:"Tick_size"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~tick_type:(fields_value (required Type.val_type))
            ~size:(fields_value (required int)))
          (fun t -> `Args $ t.tick_type $ t.size)))

  let pp ppf t =
    Format.fprintf ppf "type=%s size=%i"
      (Type.sexp_of_t t.tick_type |> Sexp.to_string_hum)
      t.size
end

module Tick_option = struct
  module Type = struct
    type t = Bid | Ask | Last | Model | Custom with sexp

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
    { tick_type : Type.t;
      implied_volatility : float option;
      delta : float option;
      option_price : Price.t option;
      pv_dividend : float option;
      gamma : float option;
      vega : float option;
      theta : float option;
      underlying_price : Price.t option;
    } with sexp, fields

  let create = Fields.create

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~tick_type:(use (=))
      ~implied_volatility:(use (Option.equal Float.(=.)))
      ~delta:(use (Option.equal Float.(=.)))
      ~option_price:(use (Option.equal Price.(=.)))
      ~pv_dividend:(use (Option.equal Float.(=.)))
      ~gamma:(use (Option.equal Float.(=.)))
      ~vega:(use (Option.equal Float.(=.)))
      ~theta:(use (Option.equal Float.(=.)))
      ~underlying_price:(use (Option.equal Price.(=.)))

  let unpickler =
    Unpickler.create ~name:"Tick_option"
      Unpickler.Spec.(
        Fields.fold
          ~init:(step Fn.id)
          ~tick_type:(fields_value (required Type.val_type))
          ~implied_volatility:(fields_value (required float))
          ~delta:(fields_value (required float))
          ~option_price:(fields_value (required Price.val_type))
          ~pv_dividend:(fields_value (required float))
          ~gamma:(fields_value (required float))
          ~vega:(fields_value (required float))
          ~theta:(fields_value (required float))
          ~underlying_price:(fields_value (required Price.val_type)))
      (fun tick_type implied_volatility delta option_price pv_dividend
        gamma vega theta underlying_price ->
          let abs = Float.abs in
          { tick_type;
            implied_volatility = if implied_volatility < 0. then None
              else Some implied_volatility;
            delta = if abs delta > 1. then None else Some delta;
            option_price = if Price.to_float option_price < 0. then None
              else Some option_price;
            pv_dividend = if pv_dividend < 0. then None else Some pv_dividend;
            gamma = if abs gamma > 1. then None else Some gamma;
            vega = if abs vega > 1. then None else Some vega;
            theta = if abs theta > 1. then None else Some theta;
            underlying_price = if Price.to_float underlying_price < 0. then None
              else Some underlying_price;
          })

  let pickler = Only_in_test.of_thunk (fun () ->
    Pickler.create ~name:"Tick_option"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~tick_type:(fields_value (required Type.val_type))
            ~implied_volatility:(fields_value (required float))
            ~delta:(fields_value (required float))
            ~option_price:(fields_value (required Price.val_type))
            ~pv_dividend:(fields_value (required float))
            ~gamma:(fields_value (required float))
            ~vega:(fields_value (required float))
            ~theta:(fields_value (required float))
            ~underlying_price:(fields_value (required Price.val_type)))
          (fun t ->
            `Args
              $ t.tick_type
              $ Option.value ~default:(-1.) t.implied_volatility
              $ Option.value ~default:(-2.) t.delta
              $ Option.value ~default:(Price.of_float (-1.)) t.option_price
              $ Option.value ~default:(-1.) t.pv_dividend
              $ Option.value ~default:(-2.) t.gamma
              $ Option.value ~default:(-2.) t.vega
              $ Option.value ~default:(-2.) t.theta
              $ Option.value ~default:(Price.of_float (-1.)) t.underlying_price)))

  let pp ppf t =
    let float_to_string x = sprintf "%4.2f" x in
    let price_to_string x = float_to_string (Price.to_float x) in
    Format.fprintf ppf
      "type=%s vol=%s delta=%sf gamma=%s vega=%s theta=%s \
       opt_price=%s pv_dividend=%s und_price=%s"
      (Type.sexp_of_t t.tick_type |> Sexp.to_string_hum)
      (Option.value_map t.implied_volatility ~default:"n/a" ~f:float_to_string)
      (Option.value_map t.delta ~default:"n/a" ~f:float_to_string)
      (Option.value_map t.gamma ~default:"n/a" ~f:float_to_string)
      (Option.value_map t.vega ~default:"n/a" ~f:float_to_string)
      (Option.value_map t.theta ~default:"n/a" ~f:float_to_string)
      (Option.value_map t.option_price ~default:"n/a" ~f:price_to_string)
      (Option.value_map t.pv_dividend ~default:"n/a" ~f:float_to_string)
      (Option.value_map t.underlying_price ~default:"n/a" ~f:price_to_string)
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
    with sexp

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
    { tick_type : Type.t;
      value : string;
    } with fields, sexp

  let create = Fields.create

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~tick_type:(use (=))
      ~value:(use (=))

  let unpickler =
    Unpickler.create ~name:"Tick_string"
      Unpickler.Spec.(
        Fields.fold
          ~init:(step Fn.id)
          ~tick_type:(fields_value (required Type.val_type))
          ~value:(fields_value (required string)))
      (fun tick_type value -> { tick_type; value })

  let pickler = Only_in_test.of_thunk (fun () ->
    Pickler.create ~name:"Tick_string"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~tick_type:(fields_value (required Type.val_type))
            ~value:(fields_value (required string)))
          (fun t -> `Args $ t.tick_type $ t.value)))

  let pp ppf t =
    Format.fprintf ppf "type=%s value=%s"
      (Type.sexp_of_t t.tick_type |> Sexp.to_string_hum)
      begin match t.tick_type with
      | Type.Last_timestamp ->
        Time.to_string_trimmed (Time.of_float (Float.of_string t.value))
      | _ -> t.value
      end
end

(* +-----------------------------------------------------------------------+
   | Orders                                                                |
   +-----------------------------------------------------------------------+ *)

module Next_order_id = struct
  type t = Raw_order.Id.t with sexp
  let create ~order_id = order_id
  let ( = ) t1 t2 = (t1 = t2)
  let unpickler =
    Unpickler.create ~name:"Next_order_id"
      Unpickler.Spec.(
        value (required Raw_order.Id.val_type)
          ~name:(field_name (Raw_order.Fields.order_id))
      ) Fn.id

  let pickler = Only_in_test.of_thunk (fun () ->
    Pickler.create ~name:"Next_order_id"
      Pickler.Spec.(value (required Raw_order.Id.val_type)))
end

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
    ] with sexp

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
    { state : State.t;
      filled : int;
      remaining : int;
      average_fill_price : Price.t;
      permanent_id : int;
      parent_id : Raw_order.Id.t;
      last_fill_price : Price.t;
      client_id : Client_id.t;
      why_held : string option;
    }
  with sexp, fields

  let create = Fields.create

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~state:(use (=))
      ~filled:(use (=))
      ~remaining:(use (=))
      ~average_fill_price:(use Price.(=.))
      ~permanent_id:(use (=))
      ~parent_id:(use Raw_order.Id.(=))
      ~last_fill_price:(use Price.(=.))
      ~client_id:(use Client_id.(=))
      ~why_held:(use (=))

  let unpickler =
    Unpickler.create ~name:"Order_status"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~state:(fields_value (required State.val_type))
          ~filled:(fields_value (required int))
          ~remaining:(fields_value (required int))
          ~average_fill_price:(fields_value (required Price.val_type))
          ~permanent_id:(fields_value (required int))
          ~parent_id:(fields_value (required Raw_order.Id.val_type))
          ~last_fill_price:(fields_value (required Price.val_type))
          ~client_id:(fields_value (required Client_id.val_type))
          ~why_held:(fields_value (optional string)))
      (fun state filled remaining average_fill_price permanent_id parent_id
        last_fill_price client_id why_held ->
          { state;
            filled;
            remaining;
            average_fill_price;
            permanent_id;
            parent_id;
            last_fill_price;
            client_id;
            why_held;
          })

  let pickler = Only_in_test.of_thunk (fun () ->
    Pickler.create ~name:"Order_status"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~state:(fields_value (required State.val_type))
            ~filled:(fields_value (required int))
            ~remaining:(fields_value (required int))
            ~average_fill_price:(fields_value (required Price.val_type))
            ~permanent_id:(fields_value (required int))
            ~parent_id:(fields_value (required Raw_order.Id.val_type))
            ~last_fill_price:(fields_value (required Price.val_type))
            ~client_id:(fields_value (required Client_id.val_type))
            ~why_held:(fields_value (optional string)))
          (fun t ->
            `Args
              $ t.state
              $ t.filled
              $ t.remaining
              $ t.average_fill_price
              $ t.permanent_id
              $ t.parent_id
              $ t.last_fill_price
              $ t.client_id
              $ t.why_held)))
end

(* +-----------------------------------------------------------------------+
   | Account and Portfolio                                                 |
   +-----------------------------------------------------------------------+ *)

(* +-----------------------------------------------------------------------+
   | Contract specs                                                        |
   +-----------------------------------------------------------------------+ *)

module Contract_specs = struct
  type t =
    { symbol : Symbol.t;
      contract_type : Contract.Type.t;
      expiry : Date.t option;
      strike : Price.t option;
      option_right : [ `Call | `Put ] option;
      exchange : Exchange.t;
      currency : Currency.t;
      local_symbol : Symbol.t option;
      market_name : string;
      trading_class : string;
      contract_id : Contract_id.t;
      min_tick : float;
      multiplier : string option;
      order_types : string list;
      valid_exchanges : Exchange.t list;
      price_magnifier : int;
      underlying_id : int;
      long_name : string;
      listing_exchange : Exchange.t option;
      contract_month : string;
      industry : string;
      category : string;
      subcategory : string;
      timezone_id : string;
      trading_hours : string;
      liquid_hours : string;
    } with sexp, fields

  let create = Fields.create

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~symbol:(use Symbol.(=))
      ~contract_type:(use (=))
      ~expiry:(use (=))
      ~strike:(use (Option.equal Price.(=.)))
      ~option_right:(use (=))
      ~exchange:(use (=))
      ~currency:(use (=))
      ~local_symbol:(use (Option.equal Symbol.(=)))
      ~market_name:(use (=))
      ~trading_class:(use (=))
      ~contract_id:(use Contract_id.(=))
      ~min_tick:(use Float.(=.))
      ~multiplier:(use (=))
      ~order_types:(use List.equal ~equal:String.(=))
      ~valid_exchanges:(use (List.equal ~equal:(=)))
      ~price_magnifier:(use (=))
      ~underlying_id:(use (=))
      ~long_name:(use (=))
      ~listing_exchange:(use (Option.equal (=)))
      ~contract_month:(use (=))
      ~industry:(use (=))
      ~category:(use (=))
      ~subcategory:(use (=))
      ~timezone_id:(use (=))
      ~trading_hours:(use (=))
      ~liquid_hours:(use (=))

  let unpickler =
    Unpickler.create ~name:"Contract_specs"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~symbol:(fields_value (required Symbol.val_type))
          ~contract_type:(fields_value (required Raw_contract.Type.val_type))
          ~expiry:(fields_value (optional date))
          ~strike:(fields_value (optional Price.val_type))
          ~option_right:(fields_value (optional Raw_contract.Option_right.val_type))
          ~exchange:(fields_value (required Exchange.val_type))
          ~currency:(fields_value (required Currency.val_type))
          ~local_symbol:(fields_value (optional Symbol.val_type))
          ~market_name:(fields_value (required string))
          ~trading_class:(fields_value (required string))
          ~contract_id:(fields_value (required Raw_contract.Id.val_type))
          ~min_tick:(fields_value (required float))
          ~multiplier:(fields_value (optional string))
          ~order_types:(fields_value (required string))
          ~valid_exchanges:(fields_value (required string))
          ~price_magnifier:(fields_value (required int))
          ~underlying_id:(fields_value (required int))
          ~long_name:(fields_value (required string))
          ~listing_exchange:(fields_value (optional Exchange.val_type))
          ~contract_month:(fields_value (required string))
          ~industry:(fields_value (required string))
          ~category:(fields_value (required string))
          ~subcategory:(fields_value (required string))
          ~timezone_id:(fields_value (required string))
          ~trading_hours:(fields_value (required string))
          ~liquid_hours:(fields_value (required string)))
      (fun symbol contract_type expiry strike option_right exchange currency
        local_symbol market_name trading_class contract_id min_tick multiplier
        order_types valid_exchanges price_magnifier underlying_id long_name
        listing_exchange contract_month industry category subcategory
        timezone_id trading_hours liquid_hours ->
          { symbol;
            contract_type;
            expiry;
            strike;
            option_right;
            exchange;
            currency;
            local_symbol;
            market_name;
            trading_class;
            contract_id;
            min_tick;
            multiplier;
            order_types =
              if String.is_empty order_types then []
              else String.split order_types ~on:',';
            valid_exchanges =
              if String.is_empty valid_exchanges then []
              else begin
                String.split valid_exchanges ~on:','
                |> List.map ~f:Exchange.t_of_tws
              end;
            price_magnifier;
            underlying_id;
            long_name;
            listing_exchange;
            contract_month;
            industry;
            category;
            subcategory;
            timezone_id;
            trading_hours;
            liquid_hours;
          })

  let pickler = Only_in_test.of_thunk (fun () ->
    Pickler.create ~name:"Contract_specs"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~symbol:(fields_value (required Symbol.val_type))
            ~contract_type:(fields_value (required Raw_contract.Type.val_type))
            ~expiry:(fields_value (optional date))
            ~strike:(fields_value (optional Price.val_type))
            ~option_right:(fields_value (optional Raw_contract.Option_right.val_type))
            ~exchange:(fields_value (required Exchange.val_type))
            ~currency:(fields_value (required Currency.val_type))
            ~local_symbol:(fields_value (optional Symbol.val_type))
            ~market_name:(fields_value (required string))
            ~trading_class:(fields_value (required string))
            ~contract_id:(fields_value (required Raw_contract.Id.val_type))
            ~min_tick:(fields_value (required float))
            ~multiplier:(fields_value (optional string))
            ~order_types:(fields_value (required string))
            ~valid_exchanges:(fields_value (required string))
            ~price_magnifier:(fields_value (required int))
            ~underlying_id:(fields_value (required int))
            ~long_name:(fields_value (required string))
            ~listing_exchange:(fields_value (optional Exchange.val_type))
            ~contract_month:(fields_value (required string))
            ~industry:(fields_value (required string))
            ~category:(fields_value (required string))
            ~subcategory:(fields_value (required string))
            ~timezone_id:(fields_value (required string))
            ~trading_hours:(fields_value (required string))
            ~liquid_hours:(fields_value (required string)))
          (fun t ->
            `Args
              $ t.symbol
              $ t.contract_type
              $ t.expiry
              $ t.strike
              $ t.option_right
              $ t.exchange
              $ t.currency
              $ t.local_symbol
              $ t.market_name
              $ t.trading_class
              $ t.contract_id
              $ t.min_tick
              $ t.multiplier
              $ (String.concat t.order_types ~sep:",")
              $ (List.map t.valid_exchanges ~f:Exchange.tws_of_t
                 |> String.concat ~sep:",")
              $ t.price_magnifier
              $ t.underlying_id
              $ t.long_name
              $ t.listing_exchange
              $ t.contract_month
              $ t.industry
              $ t.category
              $ t.subcategory
              $ t.timezone_id
              $ t.trading_hours
              $ t.liquid_hours)))

  let to_contract t =
    Contract.of_raw ({
      Raw_contract.
      contract_id = Some t.contract_id;
      symbol = t.symbol;
      contract_type = Raw_contract.Type.tws_of_t t.contract_type;
      expiry = t.expiry;
      strike = t.strike;
      option_right = t.option_right;
      multiplier = t.multiplier;
      exchange = t.exchange;
      listing_exchange = t.listing_exchange;
      currency = t.currency;
      local_symbol = t.local_symbol;
      include_expired = true;
      security_id_type = None;
      security_id = None;
      combo_legs = 0;
    })
end

(* +-----------------------------------------------------------------------+
   | Executions                                                            |
   +-----------------------------------------------------------------------+ *)

module Execution_report = struct
  module Side = struct
    type t = [ `Purchase | `Sale ] with sexp

    let tws_of_t = function
      | `Purchase -> "BOT"
      | `Sale     -> "SLD"

    let t_of_tws = function
      | "BOT" -> `Purchase
      | "SLD" -> `Sale
      | s -> invalid_argf "Side.t_of_tws: %S" s ()

    let val_type = Val_type.create tws_of_t t_of_tws

    let to_string = function
      | `Purchase -> "purchase"
      | `Sale -> "sale"

    let of_string = function
      | "purchase" -> `Purchase
      | "sale" -> `Sale
      | s -> invalid_argf "Side.of_string: %S" s ()
  end

  type t =
    { order_id : Raw_order.Id.t;
      contract : Contract.Type.t Contract.t;
      exec_id : Execution_id.t;
      time : Time.t;
      account_code : Account_code.t;
      exchange : Exchange.t;
      side : Side.t;
      quantity : int;
      price : Price.t;
      permanent_id : int;
      client_id : Client_id.t;
      liquidation : int;
      cumulative_quantity : int;
      average_price : Price.t;
      order_ref : string option;
    } with fields, sexp

  let create = Fields.create

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~order_id:(use Raw_order.Id.(=))
      ~contract:(use Contract.(=))
      ~exec_id:(use Execution_id.(=))
      ~time:(use Time.(=))
      ~account_code:(use Account_code.(=))
      ~exchange:(use (=))
      ~side:(use (=))
      ~quantity:(use (=))
      ~price:(use Price.(=.))
      ~permanent_id:(use (=))
      ~client_id:(use Client_id.(=))
      ~liquidation:(use (=))
      ~cumulative_quantity:(use (=))
      ~average_price:(use Price.(=.))
      ~order_ref:(use (=))

  let unpickler =
    let contract_spec =
      Unpickler.Spec.(
        step (fun conv id symbol contract_type expiry strike option_right
          exchange currency local_symbol ->
            let contract = Raw_contract.create ?id ~contract_type ?expiry ?strike
              ?option_right ~exchange ~currency ?local_symbol symbol
            in
            conv (Contract.of_raw contract)
        )
        ++ value (optional Raw_contract.Id.val_type)
          ~name:(field_name Raw_contract.Fields.contract_id)
        ++ value (required Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.symbol)
        ++ value (required string)
          ~name:(field_name Raw_contract.Fields.contract_type)
        ++ value (optional date)
          ~name:(field_name Raw_contract.Fields.expiry)
        ++ value (optional Price.val_type ~none_on_default:"0.0")
          ~name:(field_name Raw_contract.Fields.strike)
        ++ value (optional Raw_contract.Option_right.val_type)
          ~name:(field_name Raw_contract.Fields.option_right)
        ++ value (required Exchange.val_type)
          ~name:(field_name Raw_contract.Fields.exchange)
        ++ value (required Currency.val_type)
          ~name:(field_name Raw_contract.Fields.currency)
        ++ value (optional Symbol.val_type)
          ~name:(field_name Raw_contract.Fields.local_symbol))
    in
    Unpickler.create ~name:"Execution"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~order_id:(fields_value (required Raw_order.Id.val_type))
          ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
          ~exec_id:(fields_value (required Execution_id.val_type))
          ~time:(fields_value (required Timestamp.val_type))
          ~account_code:(fields_value (required Account_code.val_type))
          ~exchange:(fields_value (required Exchange.val_type))
          ~side:(fields_value (required Side.val_type))
          ~quantity:(fields_value (required int))
          ~price:(fields_value (required Price.val_type))
          ~permanent_id:(fields_value (required int))
          ~client_id:(fields_value (required Client_id.val_type))
          ~liquidation:(fields_value (required int))
          ~cumulative_quantity:(fields_value (required int))
          ~average_price:(fields_value (required Price.val_type))
          ~order_ref:(fields_value (optional string)))
      (fun order_id contract exec_id time account_code exchange side
        quantity price permanent_id client_id liquidation cumulative_quantity
        average_price order_ref ->
          { order_id;
            contract;
            exec_id;
            time;
            account_code;
            exchange;
            side;
            quantity;
            price;
            permanent_id;
            client_id;
            liquidation;
            cumulative_quantity;
            average_price;
            order_ref;
          })

  let pickler = Only_in_test.of_thunk (fun () ->
    let contract_spec =
      Pickler.Spec.(
        Raw_contract.Fields.fold
          ~init:(empty ())
          ~contract_id:(fields_value (optional Raw_contract.Id.val_type))
          ~symbol:(fields_value (required Symbol.val_type))
          ~contract_type:(fields_value (required string))
          ~expiry:(fields_value (optional date))
          ~strike:(fields_value (optional Price.val_type ~default_on_none:"0.0"))
          ~option_right:(fields_value (optional Raw_contract.Option_right.val_type))
          ~multiplier:(fields_value skipped)
          ~exchange:(fields_value (required Exchange.val_type))
          ~listing_exchange:(fields_value skipped)
          ~currency:(fields_value (required Currency.val_type))
          ~local_symbol:(fields_value (optional Symbol.val_type))
          ~include_expired:(fields_value skipped)
          ~security_id_type:(fields_value skipped)
          ~security_id:(fields_value skipped)
          ~combo_legs:(fields_value skipped))
      |> wrap_contract_spec
    in
    Pickler.create ~name:"Execution"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~order_id:(fields_value (required Raw_order.Id.val_type))
            ~contract:(fun specs -> Fn.const (specs ++ contract_spec))
            ~exec_id:(fields_value (required Execution_id.val_type))
            ~time:(fields_value (required Timestamp.val_type))
            ~account_code:(fields_value (required Account_code.val_type))
            ~exchange:(fields_value (required Exchange.val_type))
            ~side:(fields_value (required Side.val_type))
            ~quantity:(fields_value (required int))
            ~price:(fields_value (required Price.val_type))
            ~permanent_id:(fields_value (required int))
            ~client_id:(fields_value (required Client_id.val_type))
            ~liquidation:(fields_value (required int))
            ~cumulative_quantity:(fields_value (required int))
            ~average_price:(fields_value (required Price.val_type))
            ~order_ref:(fields_value (optional string)))
          (fun t ->
            `Args
              $ t.order_id
              $ (Contract.to_raw t.contract)
              $ t.exec_id
              $ t.time
              $ t.account_code
              $ t.exchange
              $ t.side
              $ t.quantity
              $ t.price
              $ t.permanent_id
              $ t.client_id
              $ t.liquidation
              $ t.cumulative_quantity
              $ t.average_price
              $ t.order_ref)))
end

module Commission_report = struct
  type t =
    { exec_id : Execution_id.t;
      commission : Price.t;
      currency : Currency.t;
      realized_pnl : Price.t;
      yield : float;
      yield_redemption_date : int option;
    } with sexp, fields

  let create = Fields.create

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~exec_id:(use (=))
      ~commission:(use Price.(=.))
      ~currency:(use (=))
      ~realized_pnl:(use Price.(=.))
      ~yield:(use Float.(=.))
      ~yield_redemption_date:(use (=))

  let unpickler =
    Unpickler.create ~name:"Commission_report"
      Unpickler.Spec.(
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
          { exec_id;
            commission;
            currency;
            realized_pnl;
            yield;
            yield_redemption_date;
          })

  let pickler = Only_in_test.of_thunk (fun () ->
    Pickler.create ~name:"Commission_report"
      Pickler.Spec.(
        wrap (
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
    type t = Insert | Update | Delete with sexp

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
    type t = Ask | Bid with sexp

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
    { position : int;
      operation : Operation.t;
      side : Side.t;
      price : Price.t;
      size : int;
    } with sexp, fields

  let create = Fields.create

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~position:(use (=))
      ~operation:(use (=))
      ~side:(use (=))
      ~price:(use Price.(=.))
      ~size:(use (=))

  let unpickler =
    Unpickler.create ~name:"Book_update"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~position:(fields_value (required int))
          ~operation:(fields_value (required Operation.val_type))
          ~side:(fields_value (required Side.val_type))
          ~price:(fields_value (required Price.val_type))
          ~size:(fields_value (required int)))
      (fun position operation side price size ->
        { position; operation; side; price; size })

  let pickler = Only_in_test.of_thunk (fun () ->
    Pickler.create ~name:"Book_update"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~position:(fields_value (required int))
            ~operation:(fields_value (required Operation.val_type))
            ~side:(fields_value (required Side.val_type))
            ~price:(fields_value (required Price.val_type))
            ~size:(fields_value (required int)))
          (fun t ->
            `Args $ t.position $ t.operation $ t.side $ t.price $ t.size)))
end

(* +-----------------------------------------------------------------------+
   | Historical data                                                       |
   +-----------------------------------------------------------------------+ *)

module Historical_data = struct
  module Bar = struct
    type t =
      { timestamp : Time.t;
        open_ : Price.t;
        high : Price.t;
        low : Price.t;
        close : Price.t;
        volume : int;
        wap : Price.t;
        has_gaps : bool;
        count : int;
      } with sexp, fields

    let create = Fields.create

    let ( = ) t1 t2 : bool =
      let use op = fun field ->
        op (Field.get field t1) (Field.get field t2)
      in
      Fields.for_all
        ~timestamp:(use Time.(=))
        ~open_:(use Price.(=.))
        ~high:(use Price.(=.))
        ~low:(use Price.(=.))
        ~close:(use Price.(=.))
        ~volume:(use (=))
        ~wap:(use Price.(=.))
        ~has_gaps:(use (=))
        ~count:(use (=))

    let unpickler =
      Unpickler.create ~name:"Historical_data.Bar"
        Unpickler.Spec.(
          Fields.fold
            ~init:(empty ())
            ~timestamp:(fields_value (required Timestamp.val_type))
            ~open_:(fields_value (required Price.val_type))
            ~high:(fields_value (required Price.val_type))
            ~low:(fields_value (required Price.val_type))
            ~close:(fields_value (required Price.val_type))
            ~volume:(fields_value (required int))
            ~wap:(fields_value (required Price.val_type))
            ~has_gaps:(fields_value (required string))
            ~count:(fields_value (required int)))
        (fun timestamp open_ high low close volume wap has_gaps count ->
          { timestamp;
            open_;
            high;
            low;
            close;
            volume;
            wap;
            has_gaps = Bool.of_string has_gaps;
            count;
          })

    let pickler = Only_in_test.of_thunk (fun () ->
      Pickler.create ~name:"Historical_data.Bar"
        Pickler.Spec.(
          wrap (
            Fields.fold
              ~init:(empty ())
              ~timestamp:(fields_value (required Timestamp.val_type))
              ~open_:(fields_value (required Price.val_type))
              ~high:(fields_value (required Price.val_type))
              ~low:(fields_value (required Price.val_type))
              ~close:(fields_value (required Price.val_type))
              ~volume:(fields_value (required int))
              ~wap:(fields_value (required Price.val_type))
              ~has_gaps:(fields_value (required string))
              ~count:(fields_value (required int)))
            (fun t ->
              `Args
                $ t.timestamp
                $ t.open_
                $ t.high
                $ t.low
                $ t.close
                $ t.volume
                $ t.wap
                $ Bool.to_string t.has_gaps
                $ t.count)))
  end

  type t =
    { start_time : Time.t;
      end_time : Time.t;
      num_bars : int;
      bars : Bar.t list;
    } with sexp, fields

  let create ~start_time ~end_time ~bars =
    { start_time;
      end_time;
      num_bars = List.length bars;
      bars;
    }

  let ( = ) t1 t2 =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~start_time:(use (=))
      ~end_time:(use (=))
      ~num_bars:(use (=))
      ~bars:(use (List.for_all2_exn ~f:Bar.(=)))

  let unpickler =
    Unpickler.create ~name:"Historical_data"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~start_time:(fields_value (required Timestamp.val_type))
          ~end_time:(fields_value (required Timestamp.val_type))
          ~num_bars:(fields_value (required int))
          ~bars:(fun specs -> Fn.const (specs ++ capture_remaining_message)))
      (fun start_time end_time num_bars bars_msg ->
        let num_fields = 9 in
        let bars_msg = Queue.to_array bars_msg in
        let bars = List.map (List.range 0 num_bars) ~f:(fun i ->
          let bar_msg =
            Array.sub bars_msg ~pos:(num_fields * i) ~len:num_fields
            |> Queue.of_array
          in
          Unpickler.run_exn Bar.unpickler bar_msg)
        in
        { start_time;
          end_time;
          num_bars;
          bars
        })

  let pickler = Only_in_test.of_thunk (fun () ->
    Pickler.create ~name:"Historical_data"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~start_time:(fields_value (required Timestamp.val_type))
            ~end_time:(fields_value (required Timestamp.val_type))
            ~num_bars:(fields_value (required int))
            ~bars:(fields_value tws_data))
          (fun t ->
            let pickler = Only_in_test.force Bar.pickler in
            let bars_msg =
              List.map t.bars ~f:(fun bar -> Pickler.run pickler bar)
              |> String.concat
            in
            `Args
              $ t.start_time
              $ t.end_time
              $ t.num_bars
              $ bars_msg)))

  module Columns = struct
    type t =
      { timestamps : Time.t array;
        open_prices : float array;
        high_prices : float array;
        low_prices : float array;
        close_prices : float array;
        volume : int array;
      } with sexp, fields
  end

  let to_columns t =
    let timestamps   = Array.create ~len:t.num_bars Time.epoch in
    let open_prices  = Array.create ~len:t.num_bars Float.nan in
    let high_prices  = Array.create ~len:t.num_bars Float.nan in
    let low_prices   = Array.create ~len:t.num_bars Float.nan in
    let close_prices = Array.create ~len:t.num_bars Float.nan in
    let volume       = Array.create ~len:t.num_bars 0 in
    List.iteri t.bars ~f:(fun i bar ->
      Array.set timestamps   i bar.Bar.timestamp;
      Array.set open_prices  i (Price.to_float bar.Bar.open_);
      Array.set high_prices  i (Price.to_float bar.Bar.high);
      Array.set low_prices   i (Price.to_float bar.Bar.low);
      Array.set close_prices i (Price.to_float bar.Bar.close);
      Array.set volume       i bar.Bar.volume);
    { Columns.
      timestamps;
      open_prices;
      high_prices;
      low_prices;
      close_prices;
      volume;
    }
end

(* +-----------------------------------------------------------------------+
   | Realtime bars                                                         |
   +-----------------------------------------------------------------------+ *)

module Realtime_bar = struct
  type t =
    { timestamp : Time.t;
      open_ : Price.t;
      high : Price.t;
      low : Price.t;
      close : Price.t;
      volume : int;
      wap : Price.t;
      count : int;
    } with sexp, fields

  let create = Fields.create

  let ( = ) t1 t2 : bool =
    let use op = fun field ->
      op (Field.get field t1) (Field.get field t2)
    in
    Fields.for_all
      ~timestamp:(use Time.(=))
      ~open_:(use Price.(=.))
      ~high:(use Price.(=.))
      ~low:(use Price.(=.))
      ~close:(use Price.(=.))
      ~volume:(use (=))
      ~wap:(use Price.(=.))
      ~count:(use (=))

  let unpickler =
    Unpickler.create ~name:"Realtime_bar"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~timestamp:(fields_value (required Timestamp.val_type))
          ~open_:(fields_value (required Price.val_type))
          ~high:(fields_value (required Price.val_type))
          ~low:(fields_value (required Price.val_type))
          ~close:(fields_value (required Price.val_type))
          ~volume:(fields_value (required int))
          ~wap:(fields_value (required Price.val_type))
          ~count:(fields_value (required int)))
      (fun timestamp open_ high low close volume wap count ->
          { timestamp;
            open_;
            high;
            low;
            close;
            volume;
            wap;
            count;
          })

  let pickler = Only_in_test.of_thunk (fun () ->
    Pickler.create ~name:"Realtime_bar"
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~timestamp:(fields_value (required Timestamp.val_type))
            ~open_:(fields_value (required Price.val_type))
            ~high:(fields_value (required Price.val_type))
            ~low:(fields_value (required Price.val_type))
            ~close:(fields_value (required Price.val_type))
            ~volume:(fields_value (required int))
            ~wap:(fields_value (required Price.val_type))
            ~count:(fields_value (required int)))
          (fun t ->
            `Args
              $ t.timestamp
              $ t.open_
              $ t.high
              $ t.low
              $ t.close
              $ t.volume
              $ t.wap
              $ t.count)))
end
