(* File: bar.mli

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

(* A bar contained in [History] and [Realtime_bars] responses. *)
type t = private
  { stamp : Time.t;
    (* Timestamp of the bar. *)
    op : Price.t;
    (* The opening price of the bar. *)
    hi : Price.t;
    (* The highest price during the time covered by the bar. *)
    lo : Price.t;
    (* The lowest price during the time covered by the bar. *)
    cl : Price.t;
    (* The closing price of the bar. *)
    vo : Volume.t;
    (* The volume during the time covered by the bar. *)
    wap : Price.t;
    (* The weigthed average price during the time covered by the bar. *)
    has_gaps : bool;
    (* Determines whether or not there are gaps in the data. *)
    n_trades : int;
    (* The number of trades during the time covered by the bar.  Only set when
       the [tick_type] was [Trades]. *)
  } [@@deriving sexp, fields]

include Raw_bar_intf.S
  with type raw := Raw_bar.t
  with type t := t

(** Creates a new bar from the given arguments. *)
val create
  :  stamp:Time.t
  -> op:Price.t
  -> hi:Price.t
  -> lo:Price.t
  -> cl:Price.t
  -> vo:Volume.t
  -> wap:Price.t
  -> has_gaps:bool
  -> n_trades:int
  -> t

(** Checks two bars for equality. *)
val ( = ) : t -> t -> bool

(** [combine t ~bar] combines [t] and [bar] into a new bar whose size is the
    sum of the sizes of these bars, e.g. two 1 min bars become a 2 min bar. *)
val combine : t -> bar:t -> t

(** Pretty printer for bars. *)
val pp : Format.formatter -> t -> unit
