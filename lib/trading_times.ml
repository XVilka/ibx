open Core
open Tws_prot

type t =
  { date  : Date.t
  ; hours : Time.Ofday.t list
  } [@@deriving sexp]

let create ~date ~hours = { date; hours }

let date t = t.date
let closed t = List.is_empty t.hours

let start t ~zone =
  Option.map (List.hd t.hours) ~f:(fun ofday ->
    Time.of_date_ofday t.date ofday ~zone)
;;

let stop t ~zone =
  Option.map (List.last t.hours) ~f:(fun ofday ->
    Time.of_date_ofday t.date ofday ~zone)
;;

let start_exn t ~zone = Option.value_exn (start t ~zone)
let stop_exn t ~zone = Option.value_exn (stop t ~zone)

let t_of_tws s =
  let ofday_of_string s =
    let x = Int.of_string s in
    Time.Ofday.create ~hr:(x / 100) ~min:(x mod 100) ()
  in
  let l = String.split_on_chars s ~on:[':';'-';','] in
  if List.length l = 2 && List.last_exn l = "CLOSED" then
    { date  = List.hd_exn l |> Date.of_string; hours = [] }
  else
    { date  = List.hd_exn l |> Date.of_string;
      hours = List.tl_exn l |> List.map ~f:ofday_of_string }
;;

let tws_of_t t =
  let ofday_to_string ofday =
    let mins =
      Time.Ofday.to_span_since_start_of_day ofday
      |> Time.Span.to_min
      |> Float.to_int
    in
    String.concat
      [ mins / 60   |> sprintf "%02d"
      ; mins mod 60 |> sprintf "%02d" ]
  in
  let date_to_string = Date.to_string_iso8601_basic in
  let ofdays_to_string ofdays =
    List.map ofdays ~f:ofday_to_string
    |> List.foldi ~init:"" ~f:(fun i out s ->
      if i mod 2 = 0 then out^","^s else out^"-"^s)
    |> String.chop_prefix_exn ~prefix:","
  in
  if List.is_empty t.hours then
    String.concat [date_to_string t.date; "CLOSED"] ~sep:":"
  else
    String.concat [ date_to_string t.date; ofdays_to_string t.hours ] ~sep:":"
;;

let val_type = Val_type.create tws_of_t t_of_tws
