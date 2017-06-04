open Core_kernel
open Tws_prot

module Header : sig
  type 'a t =
    { tag : 'a
    ; version : int
    }
  [@@deriving sexp]
end

module Ibx_error : sig
  type t =
    | Connection_closed
    | Unexpected_eof
    | Read_error of Sexp.t
    | Parse_error of Sexp.t
    | Tws_error of string
    | Unknown_response_handler of Query_id.t * Recv_tag.t * [ `Version of int ]
    | Decoder_mismatch of Sexp.t * Recv_tag.t Header.t list
    | Uncaught_exn of Sexp.t
  [@@deriving sexp]
end

module Ibx_result : sig
  type 'a t = ('a, Ibx_error.t) Core_kernel.Result.t [@@deriving sexp]
end

module Response_data : sig
  type 'a t = ([ `Cancel | `Response of 'a ]) Ibx_result.t [@@deriving sexp]
end

module Client_header : sig
  type t =
    { client_version : int
    ; client_id      : Client_id.t
    }
  [@@deriving sexp]
  include Encodable.S with type t := t
end

module Server_header : sig
  type t =
    { server_version  : int
    ; connection_time : Time.t
    }
  [@@deriving sexp]
  include Decodable.S with type t := t
end

module Query : sig
  type t =
    { tag     : Send_tag.t
    ; version : int
    ; id      : Query_id.t option
    ; data    : string
    }
  [@@deriving sexp]
  include Encodable.S with type t := t
end

module Response : sig
  type t =
    { tag      : Recv_tag.t
    ; version  : int
    ; query_id : Query_id.t option
    ; data     : raw_tws Queue.t Response_data.t
    }
  [@@deriving sexp]
  include Encodable.S with type t := t
end
