open Tws_prot

module type S = sig
  type t
  val encoder : t Encoder.t
end
