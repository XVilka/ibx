open Core
open Tws_prot

module Header = struct
  type 'a t =
    { tag : 'a
    ; version : int
    }
  [@@deriving sexp]
end

module Ibx_error = struct
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

module Ibx_result = struct
  type 'a t = ('a, Ibx_error.t) Base.Result.t [@@deriving sexp]
end

module Response_data = struct
  type 'a t = ([ `Cancel | `Response of 'a ]) Ibx_result.t [@@deriving sexp]
end

module Client_header = struct
  type t =
    { client_version : int
    ; client_id      : Client_id.t
    }
  [@@deriving fields, sexp]

  let encoder =
    Encoder.create ~name:"Ib.Client_header"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~client_version:(fields_value (required int))
            ~client_id:(fields_value (required Client_id.val_type)))
          (fun { client_version; client_id } ->
             `Args $ client_version $ client_id))
end

module Server_header = struct
  type t =
    { server_version  : int
    ; connection_time : Time_float_unix.t
    }
  [@@deriving fields, sexp]

  let decoder =
    Decoder.create ~name:"Ib.Server_header"
      Decoder.Spec.(
        Fields.fold
          ~init:(empty ())
          ~server_version:(fields_value (required int))
          ~connection_time:(fields_value (required time)))
      (fun server_version connection_time ->
         { server_version; connection_time })
end

module Query = struct
  type t =
    { tag     : Send_tag.t
    ; version : int
    ; id      : Query_id.t option
    ; data    : string
    }
  [@@deriving fields, sexp]

  let encoder =
    Encoder.create ~name:"Ib.Query"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~tag:(fields_value (required Send_tag.val_type))
            ~version:(fields_value (required int))
            ~id:(fields_value (skipped_if_none Query_id.val_type))
            ~data:(fields_value tws_data))
          (fun { tag; version; id; data } ->
             `Args $ tag $ version $ id $ data))
end

module Response = struct
  type t =
    { tag      : Recv_tag.t
    ; version  : int
    ; query_id : Query_id.t option
    ; data     : string Queue.t Response_data.t
    }
  [@@deriving fields, sexp]

  let encoder =
    Encoder.create ~name:"Ib.Response"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~tag:(fields_value (required Recv_tag.val_type))
            ~version:(fields_value (required int))
            ~query_id:(fields_value (skipped_if_none Query_id.val_type))
            ~data:(fields_value tws_data))
          (fun { tag; version; query_id; data } ->
             let tws_data =
               match data with
               | Error _
               | Ok `Cancel -> assert false
               | Ok (`Response data) ->
                 Queue.to_list data
                 |> String.concat ~sep:"\000"
                 |> Fn.flip (^) "\000"
             in
             `Args $ tag $ version $ query_id $ tws_data))
end
