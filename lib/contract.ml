open Core

type 'a t = Raw_contract.t constraint 'a = [< Security_type.t] [@@deriving sexp]

let to_raw = Fn.id
let of_raw = Fn.id

let ( = ) t1 t2 = Raw_contract.(=) (to_raw t1) (to_raw t2)

let sec_type t = Security_type.t_of_tws t.Raw_contract.sec_type
let con_id t = t.Raw_contract.con_id
let symbol t = t.Raw_contract.symbol
let exchange t = t.Raw_contract.exchange
let prim_exch t = t.Raw_contract.prim_exch
let currency t = t.Raw_contract.currency
let local_symbol t = t.Raw_contract.local_symbol

let sec_id t = Option.map t.Raw_contract.sec_id_type ~f:(function
  | `ISIN  -> `ISIN  (Option.value_exn t.Raw_contract.sec_id)
  | `CUSIP -> `CUSIP (Option.value_exn t.Raw_contract.sec_id)
  | `SEDOL -> `SEDOL (Option.value_exn t.Raw_contract.sec_id)
  | `RIC   -> `RIC   (Option.value_exn t.Raw_contract.sec_id))
;;

let right  t = Option.value_exn t.Raw_contract.right
let strike t = Option.value_exn t.Raw_contract.strike
let expiry t = Option.value_exn t.Raw_contract.expiry
let days_to_expiry t ~zone = Date.diff (expiry t) (Date.today ~zone)
let multiplier t = Option.value_exn t.Raw_contract.multiplier

let sort_by_expiry chain =
  List.sort chain ~compare:(fun c1 c2 -> Date.compare (expiry c1) (expiry c2))
;;

let group_by_expiry chain =
  List.group chain ~break:(fun c1 c2 -> Date.(expiry c1 <> expiry c2))
;;

let sort_by_strike chain =
  List.sort chain ~compare:(fun c1 c2 -> Price.compare (strike c1) (strike c2))
;;

let sort_futures_chain = sort_by_expiry

let sort_option_chain chain =
  let flat_map ~f = Fn.flip List.(>>=) f in
  sort_by_expiry chain
  |> group_by_expiry
  |> List.map ~f:sort_by_strike
  |> flat_map ~f:Fn.id
;;

let to_string t =
  let expiry_to_string d =
    sprintf "%s%d'%d"
      (Date.month d |> Month.to_string)
      (Date.day d)
      (Date.year d mod 100);
  in
  match sec_type t with
  | `Stock | `Index | `Forex ->
    Symbol.to_string t.Raw_contract.symbol
  | `Futures ->
    sprintf "%s %s"
      (Symbol.to_string t.Raw_contract.symbol)
      (Option.value_exn t.Raw_contract.expiry |> expiry_to_string)
  | `Option | `Fut_opt ->
    sprintf "%s %s %d %s"
      (Symbol.to_string t.Raw_contract.symbol)
      (Option.value_exn t.Raw_contract.expiry |> expiry_to_string)
      (Option.value_exn t.Raw_contract.strike |> Price.to_float |> Float.to_int)
      (Option.value_exn t.Raw_contract.right  |> Option_right.to_string)
;;

let stock ?con_id ?prim_exch ?local_symbol ?sec_id ?exchange ~currency symbol =
  of_raw (
    Raw_contract.create
      ?con_id
      ?prim_exch:prim_exch
      ?local_symbol
      ?sec_id_type:(Option.map sec_id ~f:Security_id.sec_id_type)
      ?sec_id:(Option.map sec_id ~f:Security_id.sec_id)
      ?exchange
      ~currency
      ~sec_type:"STK"
      symbol
  )
;;

let index ?con_id ?local_symbol ?sec_id ?exchange ~currency symbol =
  of_raw (
    Raw_contract.create
      ?con_id
      ?prim_exch:None
      ?local_symbol
      ?sec_id_type:(Option.map sec_id ~f:Security_id.sec_id_type)
      ?sec_id:(Option.map sec_id ~f:Security_id.sec_id)
      ?exchange
      ~currency
      ~sec_type:"IND"
      symbol
  )
;;

let futures ?con_id ?multiplier ?local_symbol ?sec_id ?include_expired ?exchange
    ~currency ~expiry symbol =
  of_raw (
    Raw_contract.create
      ?con_id
      ?multiplier
      ?prim_exch:None
      ?local_symbol
      ?sec_id_type:(Option.map sec_id ~f:Security_id.sec_id_type)
      ?sec_id:(Option.map sec_id ~f:Security_id.sec_id)
      ?include_expired
      ?exchange
      ~currency
      ~sec_type:"FUT"
      ~expiry
      symbol
  )
;;

let option ?con_id ?multiplier ?local_symbol ?sec_id ?exchange ~currency
    ~right ~expiry ~strike symbol =
  of_raw (
    Raw_contract.create
      ?con_id
      ?multiplier
      ?prim_exch:None
      ?local_symbol
      ?sec_id_type:(Option.map sec_id ~f:Security_id.sec_id_type)
      ?sec_id:(Option.map sec_id ~f:Security_id.sec_id)
      ?exchange
      ~currency
      ~sec_type:"OPT"
      ~expiry
      ~strike
      ~right
      symbol
  )
;;

let futures_option ?con_id ?multiplier ?local_symbol ?sec_id ?exchange ~currency
    ~right ~expiry ~strike symbol =
  of_raw (
    Raw_contract.create
      ?con_id
      ?multiplier
      ?prim_exch:None
      ?local_symbol
      ?sec_id_type:(Option.map sec_id ~f:Security_id.sec_id_type)
      ?sec_id:(Option.map sec_id ~f:Security_id.sec_id)
      ?exchange
      ~currency
      ~sec_type:"FOP"
      ~expiry
      ~strike
      ~right
      symbol
  )
;;

let forex ?con_id ?local_symbol ?sec_id ?(exchange=`IDEALPRO) ~currency symbol =
  of_raw (
    Raw_contract.create
      ?con_id
      ?prim_exch:None
      ?local_symbol
      ?sec_id_type:(Option.map sec_id ~f:Security_id.sec_id_type)
      ?sec_id:(Option.map sec_id ~f:Security_id.sec_id)
      ~exchange
      ~currency
      ~sec_type:"CASH"
      symbol
  )
;;

let underlying t =
  match Raw_contract.sec_type t with
  | "OPT" ->
    stock
      ~currency:t.Raw_contract.currency
      t.Raw_contract.symbol
  | "FOP" ->
    futures
      ~currency:t.Raw_contract.currency
      ~expiry:(Option.value_exn t.Raw_contract.expiry)
      t.Raw_contract.symbol
  | _ -> assert false
;;
