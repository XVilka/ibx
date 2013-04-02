(** Simple interface to Gnuplot. Based on code from the Gp module of
    Guillaume Hennequin <gje.hennequin@gmail.com>, which is available
    {{:http://lcn.epfl.ch/~hennequi/software.html} online}  *)

type t

val create : unit -> t
val close  : t -> unit

val send : t -> string -> unit
val plot : t -> (float array list * string) list -> unit
