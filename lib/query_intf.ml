open Core_kernel
open Tws_prot

module type S = sig
  type t
  include Sexpable.S with type t := t
  include Encodable.S with type t := t
  val decoder : t Decoder.t Only_in_test.t
  val ( = ) : t -> t -> bool
end
