(* File: tws_prot.ml

   IBX - Pure OCaml implementation of the Interactive Brokers TWS API

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

module Val_type = struct
  type 'a t = {
    to_string : 'a -> string;
    of_string : string -> 'a;
  }

  let create to_string of_string =
    { to_string; of_string }

  let unit_to_string () = ""
  let unit_of_string = function
    | "" -> ()
    | s -> invalid_argf "Val_type.unit_of_string: %S" s ()

  let unit   = create unit_to_string unit_of_string

  let string = create Fn.id Fn.id
  let int    = create Int.to_string Int.of_string
  let int64  = create Int64.to_string Int64.of_string

  let float_to_string x =
    let s = Float.to_string x in
    let n = String.length s in
    if s.[n-1] = '.' then s^"0" else s

  let float  = create float_to_string Float.of_string

  let bool_to_string = function
    | false -> "0"
    | true  -> "1"

  let bool_of_string = function
    | "0" -> false
    | "1" -> true
    | s -> invalid_argf "Val_type.bool_of_string: %S" s ()

  let bool   = create bool_to_string bool_of_string

  let time_to_string tm = Time.format tm "%Y%m%d %H:%M:%S"
  let time   = create time_to_string Time.of_string

  let date   = create Date.to_string_iso8601_basic (Date.of_string_iso8601_basic ~pos:0)
end

module Pickler = struct

  let serialize_aux string buf =
    let nul_byte = Char.of_int_exn 0 in
    Buffer.add_string buf string;
    Buffer.add_char buf nul_byte

  let serialize to_string v buf = serialize_aux (to_string v) buf

  let serialize_opt default_on_none to_string v_opt buf =
    match v_opt with
    | None   -> serialize_aux default_on_none buf
    | Some v -> serialize to_string v buf

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

    let wrap t f = {
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
      let time   = time
      let date   = date
    end

    type 'a value = {
      value : 'a -> Buffer.t -> unit;
    }

    let required val_type = {
      value = serialize val_type.Val_type.to_string;
    }

    let optional ?(default_on_none="") val_type = {
      value = serialize_opt default_on_none val_type.Val_type.to_string;
    }

    let skipped_if_none val_type = {
      value = (fun v_opt buf ->
        match v_opt with
        | None -> ()
        | Some v -> serialize val_type.Val_type.to_string v buf);
    }

    let skipped = {
      value = (fun _v _buf -> ());
    }

    let tws_data = {
      value = (fun s buf -> Buffer.add_string buf s)
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

  let run pickler value =
    let buf = Buffer.create pickler.buf_size in
    pickler.f value buf;
    Buffer.contents buf
end

module Unpickler = struct

  let parse_aux of_string name arg =
    match Result.try_with (fun () -> of_string arg) with
    | Ok value -> value
    | Error exn ->
      failwithf "failed to parse %s value %S -- %s" name arg (Exn.to_string exn) ()

  let parse_opt none_on_default of_string name msg =
    match Queue.dequeue msg with
    | None -> failwithf "missing message field %s" name ()
    | Some arg ->
      if String.equal arg none_on_default then (None, msg)
      else (Some (parse_aux of_string name arg), msg)

  let parse of_string name msg =
    match Queue.dequeue msg with
    | None -> failwithf "missing message field %s" name ()
    | Some arg -> (parse_aux of_string name arg, msg)

  module Spec = struct

    type ('a, 'b) t = {
      f : ('a Lazy.t * string Queue.t -> 'b Lazy.t * string Queue.t);
    }

    let (++) t1 t2 = {
      f = Fn.compose t2.f t1.f;
    }

    let step f = {
      f = (fun (thunk, msg) -> (lazy (f (Lazy.force thunk)), msg));
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
      let time   = time
      let date   = date
    end

    type 'a parse = string Queue.t -> 'a * string Queue.t

    type 'a value = {
      value : string -> 'a parse;
    }

    let optional ?(none_on_default="") val_type = {
      value = parse_opt none_on_default val_type.Val_type.of_string;
    }

    let required val_type = {
      value = parse val_type.Val_type.of_string;
    }

    let capture_remaining_message = {
      f = (fun (k, msg) ->
        let q = Queue.create () in
        Queue.transfer ~src:msg ~dst:q;
        (lazy (Lazy.force k q), msg));
    }

    let value v ~name = {
      f = (fun (k, msg) ->
        let (a, remaining_msg) = v.value name msg in
        (lazy (Lazy.force k a), remaining_msg));
    }

    let fields_value v specs field =
      specs ++ value v ~name:(Fieldslib.Field.name field)

  end

  type 'a t = {
    f : string Queue.t -> 'a Lazy.t;
    name : string option;
  }

  let create ?name {Spec.f} conv = {
    f = (fun msg ->
      let (thunk, remaining_msg) = f (lazy conv, msg) in
      if Queue.is_empty remaining_msg then
        thunk
      else failwiths "message is too long" (Queue.length msg) sexp_of_int);
    name;
  }

  let map t ~f = {
    f = (fun msg -> lazy (f (Lazy.force (t.f msg))));
    name = t.name;
  }

  let const a = {
    f = (fun _msg -> lazy a);
    name = None;
  }

  let run unpickler msg =
    match Or_error.try_with (fun () -> unpickler.f msg) with
    | Ok thunk -> Ok (Lazy.force thunk)
    | Error err ->
      Error (Option.value_map unpickler.name ~default:err ~f:(Error.tag err))

  let run_exn unpickler msg = Or_error.ok_exn (run unpickler msg)
end
