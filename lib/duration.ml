(* File: duration.ml

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
  [ `Sec   of int
  | `Day   of int
  | `Week  of int
  | `Month of int
  | `Year  of int
  ] with sexp
end
include T
include Sexpable.To_stringable (T)

let tws_of_t = function
  | `Sec   x -> sprintf "%d S" x
  | `Day   x -> sprintf "%d D" x
  | `Week  x -> sprintf "%d W" x
  | `Month x -> sprintf "%d M" x
  | `Year  x -> sprintf "%d Y" x

let t_of_tws s =
  let extract_int s ~time_unit =
    let pattern = "\\([1-9][0-9]*\\)  " in
    String.nset pattern 16 time_unit;
    if Str.string_match (Str.regexp pattern) s 0 then
      Int.of_string (Str.matched_group 1 s)
    else invalid_argf "Bar_span.t_of_tws: %S" s ()
  in
  match String.nget s (String.length s - 1) with
  | 'S' -> `Sec   (extract_int s ~time_unit:'S')
  | 'D' -> `Day   (extract_int s ~time_unit:'D')
  | 'W' -> `Week  (extract_int s ~time_unit:'W')
  | 'M' -> `Month (extract_int s ~time_unit:'M')
  | 'Y' -> `Year  (extract_int s ~time_unit:'Y')
  | _ -> invalid_argf "Bar_span.t_of_tws: %S" s ()

let val_type = Val_type.create tws_of_t t_of_tws
