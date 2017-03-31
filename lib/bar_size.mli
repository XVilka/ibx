(* File: bar_size.mli

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

open Core

(** Specifies the size of historical bars. *)
type t =
  [ `One_sec     | `Five_sec   | `Fifteen_sec | `Thirty_sec
  | `One_min     | `Two_min    | `Three_min   | `Five_min
  | `Fifteen_min | `Thirty_min | `One_hour    | `One_day
  ] [@@deriving sexp]
include Stringable.S with type t := t
include Twsable.S with type t := t

(** [to_span t] converts the bar size specification into a time span. *)
val to_span : t -> Time.Span.t
