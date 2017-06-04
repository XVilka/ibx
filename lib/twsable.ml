open Tws_prot

module type S = sig
  type t
  val val_type : t Val_type.t
end
