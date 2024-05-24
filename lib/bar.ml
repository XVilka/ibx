open Core
open Tws_prot

include Raw_bar

let to_raw = Fn.id
let of_raw = Fn.id

let ( = ) t1 t2 = Raw_bar.(=) (to_raw t1) (to_raw t2)

let combine t ~bar =
  let hi = Price.max t.hi bar.hi in
  let lo = Price.min t.lo bar.lo in
  let cl = bar.cl in
  { t with
    hi
  ; lo
  ; cl
  ; vo = Volume.(t.vo + bar.vo)
  ; wap = Price.((hi + lo + cl) / of_float 3.)
  ; has_gaps = t.has_gaps || bar.has_gaps
  ; n_trades = Int.(t.n_trades + bar.n_trades)
  }

let pp ppf t =
  Format.fprintf ppf
    "Bar<%s> op=%.2f hi=%.2f lo=%.2f cl=%.2f vo=%d wap=%.2f trades=%d"
    (t.stamp |> Time_float_unix.to_sec_string ~zone:(Lazy.force Time_float_unix.Zone.local))
    (t.op :> float) (t.hi :> float) (t.lo :> float) (t.cl :> float)
    (t.vo :> int) (t.wap :> float) (t.n_trades)

module Duration = struct
  module T = struct
    type t =
      [ `Sec   of int
      | `Day   of int
      | `Week  of int
      | `Month of int
      | `Year  of int
      ] [@@deriving sexp, eq]
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
end

module Size = struct
  module T = struct
    type t =
      [ `One_sec     | `Five_sec   | `Fifteen_sec | `Thirty_sec
      | `One_min     | `Two_min    | `Three_min   | `Five_min
      | `Fifteen_min | `Thirty_min | `One_hour    | `One_day
      ] [@@deriving sexp, eq]
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
    | `One_sec -> Time_float_unix.Span.second
    | `Five_sec -> Time_float_unix.Span.of_sec 5.
    | `Fifteen_sec -> Time_float_unix.Span.of_sec 15.
    | `Thirty_sec -> Time_float_unix.Span.of_sec 30.
    | `One_min -> Time_float_unix.Span.minute
    | `Two_min -> Time_float_unix.Span.of_min 2.
    | `Three_min -> Time_float_unix.Span.of_min 3.
    | `Five_min -> Time_float_unix.Span.of_min 5.
    | `Fifteen_min -> Time_float_unix.Span.of_min 15.
    | `Thirty_min -> Time_float_unix.Span.of_min 30.
    | `One_hour -> Time_float_unix.Span.hour
    | `One_day -> Time_float_unix.Span.day
end
