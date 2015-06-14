(* File: bar_size.ml

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
open Tws_prot

module T = struct
  type t =
  [ `One_sec     | `Five_sec   | `Fifteen_sec | `Thirty_sec
  | `One_min     | `Two_min    | `Three_min   | `Five_min
  | `Fifteen_min | `Thirty_min | `One_hour    | `One_day
  ] with sexp
end
include T
include Sexpable.To_stringable (T)

let tws_of_t = function
  | `One_sec -> "1 sec"
  | `Five_sec -> "5 secs"
  | `Fifteen_sec -> "15 secs"
  | `Thirty_sec -> "30 secs"
  | `One_min -> "1 min"
  | `Two_min -> "2 mins"
  | `Three_min -> "3 mins"
  | `Five_min -> "5 mins"
  | `Fifteen_min -> "15 mins"
  | `Thirty_min -> "30 mins"
  | `One_hour -> "1 hour"
  | `One_day -> "1 day"

let t_of_tws = function
  | "1 sec" -> `One_sec
  | "5 secs" -> `Five_sec
  | "15 secs" -> `Fifteen_sec
  | "30 secs" -> `Thirty_sec
  | "1 min" -> `One_min
  | "2 mins" -> `Two_min
  | "3 mins" -> `Three_min
  | "5 mins" -> `Five_min
  | "15 mins" -> `Fifteen_min
  | "30 mins" -> `Thirty_min
  | "1 hour" -> `One_hour
  | "1 day" -> `One_day
  | s -> invalid_argf "Bar_size.t_of_tws: %S" s ()

let val_type = Val_type.create tws_of_t t_of_tws

let to_span = function
  | `One_sec -> Time.Span.second
  | `Five_sec -> Time.Span.of_sec 5.
  | `Fifteen_sec -> Time.Span.of_sec 15.
  | `Thirty_sec -> Time.Span.of_sec 30.
  | `One_min -> Time.Span.minute
  | `Two_min -> Time.Span.of_min 2.
  | `Three_min -> Time.Span.of_min 3.
  | `Five_min -> Time.Span.of_min 5.
  | `Fifteen_min -> Time.Span.of_min 15.
  | `Thirty_min -> Time.Span.of_min 30.
  | `One_hour -> Time.Span.hour
  | `One_day -> Time.Span.day
