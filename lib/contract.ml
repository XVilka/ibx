(* File: contract.ml

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

include struct
  open Raw_contract
  module Id = Id
  module Option_right = Option_right
end

module Security_id = struct
  module T = struct
    type t =
    [ `ISIN  of string
    | `RIC   of string
    | `CUSIP of string
    | `SEDOL of string
    ] with sexp
  end
  include T
  include Sexpable.To_stringable (T)
end

type 'a t = Raw_contract.t constraint 'a = [< Security_type.t] with sexp

let to_raw = Fn.id
let of_raw = Fn.id

let ( = ) t1 t2 = Raw_contract.(=) (to_raw t1) (to_raw t2)

let sec_type t = Security_type.t_of_tws t.Raw_contract.sec_type
let id t = t.Raw_contract.contract_id
let symbol t = t.Raw_contract.symbol
let exchange t = t.Raw_contract.exchange
let listed_on t = t.Raw_contract.listing_exchange
let currency t = t.Raw_contract.currency
let local_symbol t = t.Raw_contract.local_symbol

let sec_id t =
  Option.map t.Raw_contract.sec_id_type ~f:(function
    | `ISIN  -> `ISIN  (Option.value_exn t.Raw_contract.sec_id)
    | `CUSIP -> `CUSIP (Option.value_exn t.Raw_contract.sec_id)
    | `SEDOL -> `SEDOL (Option.value_exn t.Raw_contract.sec_id)
    | `RIC   -> `RIC   (Option.value_exn t.Raw_contract.sec_id))

let option_right t = Option.value_exn t.Raw_contract.option_right
let strike t = Option.value_exn t.Raw_contract.strike
let expiry t = Option.value_exn t.Raw_contract.expiry
let days_to_expiry t ~zone = Date.diff (expiry t) (Date.today ~zone)
let multiplier t = Option.value_exn t.Raw_contract.multiplier
let include_expired t = t.Raw_contract.include_expired
let combo_legs t = t.Raw_contract.combo_legs

let to_string t =
  let expiry_to_string d =
    sprintf "%s%d'%d"
      (Date.month d |> Month.to_string)
      (Date.day d)
      (Date.year d mod 100);
  in
  match sec_type t with
  | `Stock | `Index | `Forex ->
    Symbol.to_string t.Raw_contract.symbol
  | `Futures ->
    sprintf "%s %s"
      (Symbol.to_string t.Raw_contract.symbol)
      (Option.value_exn t.Raw_contract.expiry |> expiry_to_string)
  | `Option | `Fut_opt ->
    sprintf "%s %s %d %s"
      (Symbol.to_string t.Raw_contract.symbol)
      (Option.value_exn t.Raw_contract.expiry |> expiry_to_string)
      (Option.value_exn t.Raw_contract.strike |> Price.to_float |> Float.to_int)
      (Option.value_exn t.Raw_contract.option_right |> Option_right.to_string)

let split_sec_id = function
  | None -> None, None
  | Some sec_id ->
    match sec_id with
    | `ISIN  x -> (Some `ISIN , Some x)
    | `RIC   x -> (Some `RIC  , Some x)
    | `CUSIP x -> (Some `CUSIP, Some x)
    | `SEDOL x -> (Some `SEDOL, Some x)

let stock ?id ?listed_on ?local_symbol ?sec_id ?exchange ~currency symbol =
  let sec_id_type, sec_id = split_sec_id sec_id in
  of_raw (
    Raw_contract.create
      ?id
      ?listing_exchange:listed_on
      ?local_symbol
      ?sec_id_type
      ?sec_id
      ?exchange
      ~currency
      ~sec_type:"STK"
      symbol
  )

let index ?id ?local_symbol ?sec_id ?exchange ~currency symbol =
  let sec_id_type, sec_id = split_sec_id sec_id in
  of_raw (
    Raw_contract.create
      ?id
      ?listing_exchange:None
      ?local_symbol
      ?sec_id_type
      ?sec_id
      ?exchange
      ~currency
      ~sec_type:"IND"
      symbol
  )

let futures ?id ?multiplier ?local_symbol ?sec_id ?include_expired ?exchange
    ~currency ~expiry symbol =
  let sec_id_type, sec_id = split_sec_id sec_id in
  of_raw (
    Raw_contract.create
      ?id
      ?multiplier
      ?listing_exchange:None
      ?local_symbol
      ?sec_id_type
      ?sec_id
      ?include_expired
      ?exchange
      ~currency
      ~sec_type:"FUT"
      ~expiry
      symbol
  )

let option ?id ?multiplier ?local_symbol ?sec_id ?exchange
    ~currency ~option_right ~expiry ~strike symbol =
  let sec_id_type, sec_id = split_sec_id sec_id in
  of_raw (
    Raw_contract.create
      ?id
      ?multiplier
      ?listing_exchange:None
      ?local_symbol
      ?sec_id_type
      ?sec_id
      ?exchange
      ~currency
      ~sec_type:"OPT"
      ~expiry
      ~strike
      ~option_right
      symbol
  )

let futures_option ?id ?multiplier ?local_symbol ?sec_id ?exchange
    ~currency ~option_right ~expiry ~strike symbol =
  let sec_id_type, sec_id = split_sec_id sec_id in
  of_raw (
    Raw_contract.create
      ?id
      ?multiplier
      ?listing_exchange:None
      ?local_symbol
      ?sec_id_type
      ?sec_id
      ?exchange
      ~currency
      ~sec_type:"FOP"
      ~expiry
      ~strike
      ~option_right
      symbol
  )

let forex ?id ?local_symbol ?sec_id ?(exchange=`IDEALPRO) ~currency symbol =
  let sec_id_type, sec_id = split_sec_id sec_id in
  of_raw (
    Raw_contract.create
      ?id
      ?listing_exchange:None
      ?local_symbol
      ?sec_id_type
      ?sec_id
      ~exchange
      ~currency
      ~sec_type:"CASH"
      symbol
  )

let underlying t =
  match Raw_contract.sec_type t with
  | "OPT" ->
    stock
      ~currency:t.Raw_contract.currency
      t.Raw_contract.symbol
  | "FOP" ->
    futures
      ~currency:t.Raw_contract.currency
      ~expiry:(Option.value_exn t.Raw_contract.expiry)
      t.Raw_contract.symbol
  | _ -> assert false
