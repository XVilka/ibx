(* File: tws_prot.ml

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

type raw_tws = string [@@deriving sexp]

module Val_type = struct
  type 'a t = {
    tws_of_a : 'a -> raw_tws;
    a_of_tws : raw_tws -> 'a;
  }

  let create tws_of_a a_of_tws = { tws_of_a; a_of_tws }

  let tws_of_unit () = ""
  let unit_of_tws = function
    | "" -> ()
    | s -> invalid_argf "Val_type.unit_of_tws: %S" s ()
  let unit = create tws_of_unit unit_of_tws

  let string = create Fn.id Fn.id
  let int    = create Int.to_string Int.of_string
  let int64  = create Int64.to_string Int64.of_string

  let tws_of_float x =
    let s = Float.to_string_round_trippable x in
    let n = String.length s in
    if s.[n-1] = '.' then s^"0" else s
  let float_of_tws = Float.of_string
  let float = create tws_of_float float_of_tws

  let tws_of_bool = function
    | false -> "0"
    | true  -> "1"
  let bool_of_tws = function
    | "0" -> false
    | "1" -> true
    | s -> invalid_argf "Val_type.bool_of_tws: %S" s ()
  let bool = create tws_of_bool bool_of_tws
  let bools = create Bool.to_string Bool.of_string

  let tws_of_stamp t =
    Time.to_span_since_epoch t
    |> Time.Span.to_proportional_float
    |> Float.to_string_round_trippable

  let stamp_of_tws s =
    Float.of_string s
    |> Unix.localtime
    |> Time.of_tm ~zone:(Lazy.force Time.Zone.local)

  let stamp = create tws_of_stamp stamp_of_tws

  let tws_of_time tm = Time.format tm "%Y%m%d %H:%M:%S"
      ~zone:(Lazy.force Time.Zone.local)

  let unescape = unstage (String.Escaping.unescape ~escape_char:' ')
  let time_of_date d =
    Time.(of_date_ofday d Ofday.start_of_day ~zone:(Lazy.force Zone.local))

  let time_of_tws s =
    let len = String.length s in
    if len = 8 then time_of_date (Date.of_string_iso8601_basic s ~pos:0)
    else if len = 18 then Time.of_string (unescape s) else Time.of_string s

  let time = create tws_of_time time_of_tws

  let tws_of_date = Date.to_string_iso8601_basic
  let date_of_tws = Date.of_string_iso8601_basic ~pos:0
  let date = create tws_of_date date_of_tws

  let time_zone_of_string = function
    | "CST" -> Time.Zone.find_exn "America/Chicago"
    (* NOTE: Futures traded in Chicago, like ES, have CST as time zone which is
       ambiguous.  Return the time zone of Chicago instead. *)
    | s -> Time.Zone.of_string s

  let zone = create Time.Zone.to_string time_zone_of_string
end

module Pickler = struct

  let serialize_aux raw_tws buf =
    Buffer.add_string buf raw_tws;
    Buffer.add_char buf '\000'

  let serialize tws_of_a a buf = serialize_aux (tws_of_a a) buf

  let serialize_opt default_on_none tws_of_a a_opt buf =
    match a_opt with
    | None   -> serialize_aux default_on_none buf
    | Some a -> serialize tws_of_a a buf

  module Spec = struct
    type 'a t = {
      f : 'a -> Buffer.t -> unit;
    }

    let (++) t1 t2 = {
      f = (fun (a, b) buf -> t1.f a buf; t2.f b buf)
    }

    let empty () = {
      f = (fun `Args _buf -> ())
    }

    let ($) x y = (x, y)

    let lift t f = {
      f = Fn.compose t.f f;
    }

    include struct
      open Val_type
      let unit   = unit
      let string = string
      let int    = int
      let int64  = int64
      let float  = float
      let bool   = bool
      let bools  = bools
      let stamp  = stamp
      let time   = time
      let date   = date
      let zone   = zone
    end

    type 'a value = {
      value : 'a -> Buffer.t -> unit;
    }

    let sequence ?(sep=',') val_type = {
      value = (fun a_list buf ->
        if List.is_empty a_list then
          serialize string.Val_type.tws_of_a "" buf
        else
          let sep = Char.to_string sep in
          serialize string.Val_type.tws_of_a
            (String.concat ~sep (List.map a_list ~f:val_type.Val_type.tws_of_a))
            buf);
    }

    let required val_type = {
      value = (fun a buf -> serialize val_type.Val_type.tws_of_a a buf);
    }

    let optional ?(default_on_none="") val_type = {
      value = (fun a buf ->
        serialize_opt default_on_none val_type.Val_type.tws_of_a a buf);
    }

    let skipped_if_none val_type = {
      value = (fun a_opt buf ->
        match a_opt with
        | None   -> ()
        | Some a -> serialize val_type.Val_type.tws_of_a a buf);
    }

    let skipped = {
      value = (fun _a _buf -> ());
    }

    let tws_data = {
      value = (fun raw_tws buf -> Buffer.add_string buf raw_tws)
    }

    let value v = {
      f = v.value;
    }

    let fields_value v specs _field = specs ++ value v
  end

  type 'a t = {
    f : 'a -> Buffer.t -> unit;
    name : string option;
    buf_size : int;
  }

  let create ?(buf_size=256) ?name {Spec.f} =
    { f; name; buf_size }

  let run t value =
    let buf = Buffer.create t.buf_size in
    t.f value buf;
    Buffer.contents buf
end

module Unpickler = struct

  let parse_aux name a_of_tws raw_tws =
    match Result.try_with (fun () -> a_of_tws raw_tws) with
    | Ok a -> a
    | Error exn ->
      failwithf "failed to parse %s value %S -- %s" name raw_tws (Exn.to_string exn) ()

  let parse name a_of_tws msg =
    match Queue.dequeue msg with
    | None -> failwithf "missing message field %s" name ()
    | Some raw_tws -> (parse_aux name a_of_tws raw_tws, msg)

  let parse_opt none_on_default name a_of_tws msg =
    match Queue.dequeue msg with
    | None -> failwithf "missing message field %s" name ()
    | Some raw_tws ->
      if String.equal raw_tws none_on_default
      then (None                                  , msg)
      else (Some (parse_aux name a_of_tws raw_tws), msg)

  module Spec = struct

    type ('a, 'b) t = {
      f : ('a * raw_tws Queue.t -> 'b * raw_tws Queue.t);
    }

    let (++) t1 t2 = {
      f = Fn.compose t2.f t1.f;
    }

    let step f = {
      f = (fun (a, msg) -> (f a, msg));
    }

    let empty () = step Fn.id

    include struct
      open Val_type
      let unit   = unit
      let string = string
      let int    = int
      let int64  = int64
      let float  = float
      let bool   = bool
      let bools  = bools
      let stamp  = stamp
      let time   = time
      let date   = date
      let zone   = zone
    end

    type 'a parse = raw_tws Queue.t -> 'a * raw_tws Queue.t

    type 'a value = {
      value : name:string -> 'a parse;
    }

    let sequence ?(sep=',') val_type = {
      value = (fun ~name msg ->
        let s, msg = parse name string.Val_type.a_of_tws msg in
        if String.is_empty s then [], msg else
          List.map (String.split s ~on:sep ) ~f:val_type.Val_type.a_of_tws, msg)
    }

    let required val_type = {
      value = (fun ~name msg -> parse name val_type.Val_type.a_of_tws msg);
    }

    let optional ?(none_on_default="") val_type = {
      value = (fun ~name msg ->
        parse_opt none_on_default name val_type.Val_type.a_of_tws msg);
    }

    let optional_with_default ~default val_type = {
      value = (fun ~name msg ->
        match parse_opt "" name val_type.Val_type.a_of_tws msg with
        | None  , msg -> default, msg
        | Some a, msg -> a      , msg);
    }

    let capture_remaining_message = {
      f = (fun (k, msg) ->
        let captured_msg = Queue.create () in
        Queue.blit_transfer ~src:msg ~dst:captured_msg ();
        (k captured_msg, msg));
    }

    let value v ~name = {
      f = (fun (k, msg) ->
        let (a, remaining_msg) = v.value msg ~name in
        (k a, remaining_msg));
    }

    let fields_value v specs field =
      specs ++ value v ~name:(Fieldslib.Field.name field)

  end

  type 'a t = {
    f : raw_tws Queue.t -> 'a;
    name : string option;
  }

  let create ?name {Spec.f} conv = {
    f = (fun msg ->
      let (result, remaining_msg) = f (conv, msg) in
      if Queue.is_empty remaining_msg
      then result
      else failwiths "message is too long" (Queue.length msg) sexp_of_int);
    name;
  }

  let map t ~f = {
    f = (fun msg -> (f (t.f msg)));
    name = t.name;
  }

  let const a = {
    f = (fun _msg -> a);
    name = None;
  }

  let run t msg =
    match Or_error.try_with (fun () -> t.f msg) with
    | Ok _ as x -> x
    | Error e ->
      Error (Option.value_map t.name ~default:e ~f:(fun tag -> Error.tag e ~tag))

  let run_exn t msg = Or_error.ok_exn (run t msg)
end
