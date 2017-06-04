open Async
open Core

module Header = struct
  let length = 4

  let unsafe_get_message_length buf ~pos =
    Bigstring.unsafe_get_uint32_be buf ~pos
  ;;

  let unsafe_set_message_length buf ~pos message_len =
    Bigstring.unsafe_set_uint32_be buf ~pos message_len
  ;;
end


let rec pipe_read t f =
  Pipe.values_available t
  >>= function
  | `Eof -> f `Eof
  | `Ok ->
    match Pipe.read_now' t with
    | `Nothing_available -> pipe_read t f
    | (`Eof | `Ok _) as x -> f x

module Pipe_and_buffer = struct
  type 'a t =
    { pipe   : 'a Pipe.Reader.t
    ; buffer : Bigbuffer.t
    } [@@deriving sexp_of]

  let create pipe =
    { pipe
    ; buffer = Bigbuffer.create Header.length
    }
  ;;
end

module Pipe_reader = struct
  type t = string Pipe_and_buffer.t [@@deriving sexp_of]

  let close (t:t) = Pipe.close_read t.pipe; Deferred.unit
  let closed (t:t) = Pipe.is_closed t.pipe

  (* let read_forever (t:t) ~on_message *)

end

module Pipe_and_monitor = struct
  type 'a t =
    { pipe    : 'a Pipe.Writer.t
    ; monitor : Monitor.t
    } [@@deriving sexp_of]

  let create pipe =
    { pipe
    ; monitor = Monitor.create ()
    }
  ;;
end

module Send_result = struct
  type message_too_big =
    { size             : int
    ; max_message_size : int
    }
  [@@deriving sexp_of]

  type 'a t =
    | Sent of 'a
    | Closed
    | Message_too_big of message_too_big
  [@@deriving sexp_of]
end

module Pipe_writer = struct
  type t = string Pipe_and_monitor.t [@@deriving sexp_of]

  let close (t:t) = Pipe.close t.pipe; Deferred.unit
  let is_closed (t:t) = Pipe.is_closed t.pipe

  let monitor (t:t) = t.monitor

  (* Because we don't maintain any buffer, there are no pending writes *)
  let bytes_to_write (_ : t) = 0

  let stopped (t:t) = Pipe.closed t.pipe

  (* We consider that a message is flushed as soon as it reaches the underlining
     transport. *)
  let flushed (_ : t) = Deferred.unit

  let ready_to_write = flushed

  let sent_result x : _ Send_result.t = Sent x

  let check_closed (t:t) f =
    if not (Pipe.is_closed t.pipe) then f () else Send_result.Closed

  let send_tws t msg =
    check_closed t (fun () ->
      let msg_length = String.length msg in
      let buf = Bigstring.create (Header.length + msg_length) in
      Header.unsafe_set_message_length buf ~pos:0 msg_length;
      Bigstring.blit ~src:(Bigstring.of_string msg) ~src_pos:0 ~dst:buf
        ~dst_pos:Header.length ~len:msg_length;
      Pipe.write_without_pushback t.pipe (Bigstring.to_string buf);
      sent_result ()
    )

end


type t =
  { reader : Reader.t
  ; writer : Writer.t
  }
[@@deriving sexp_of]


(* let create reader writer = *)
(*   { reader = Reader.pack (module Pipe_reader) reader *)
(*   ; writer = Writer.pack (module Pipe_writer) writer *)
(*   } *)
(* ;; *)
