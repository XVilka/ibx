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

type t = private
  { stamp : Time.t;
    op : Price.t;
    hi : Price.t;
    lo : Price.t;
    cl : Price.t;
    vo : Volume.t;
    wap : Price.t;
    has_gaps : bool;
    count : int;
  } with sexp, fields

include Raw_bar_intf.S
  with type raw := Raw_bar.t
  with type t := t

val create
  :  stamp:Time.t
  -> op:Price.t
  -> hi:Price.t
  -> lo:Price.t
  -> cl:Price.t
  -> vo:Volume.t
  -> wap:Price.t
  -> has_gaps:bool
  -> count:int
  -> t

val ( = ) : t -> t -> bool

val pp : Format.formatter -> t -> unit
