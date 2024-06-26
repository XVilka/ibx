open Core

(* Futher information about the exchange listings can be found
   {{:http://www.interactivebrokers.com/en/index.php?f=exchanges} online}. *)

type t =
  [ `AEB        (* Euronext NL Stocks *)
  | `ALPHA      (* Alpha ATS *)
  | `AMEX       (* NYSE AMEX *)
  | `ARCA       (* NYSE Arca *)
  | `ARCAEDGE   (* ArcaEdge *)
  | `ASX        (* Australian Stock Exchange *)
  | `BATECH     (* BATS Europe *)
  | `BATEDE     (* BATS Europe *)
  | `BATEEN     (* BATS Europe *)
  | `BATEUK     (* BATS Europe *)
  | `BATS       (* BATS Global Markets *)
  | `BEX        (* NASDAQ OMX BX *)
  | `BM         (* Bolsa de Madrid *)
  | `BOX        (* Boston Options Exchange *)
  | `BTRADE     (* Bloomberg Tradebook *)
  | `BVME       (* Borsa Italiana *)
  | `BYX        (* BATS BYX *)
  | `CBOE       (* Chicago Board Options Exchange *)
  | `CBOE2      (* CBOE C2 *)
  | `CBSX       (* CBOE Stock Exchange *)
  | `CDE        (* Montreal Exchange *)
  | `CFE        (* CBOE Futures Exchange *)
  | `CHIXCH     (* CHI-X Europe Ltd Swiss *)
  | `CHIXDE     (* CHI-X Europe LTD Clearstream *)
  | `CHIXEN     (* CHI-X Europe LTD Clearnet *)
  | `CHIXEN     (* CHI-X Europe LTD Clearnet *)
  | `CHIXJ      (* CHI-X Japan *)
  | `CHIXUK     (* CHI-X Europe Ltd Crest *)
  | `CHX        (* Chicago Stock Exchange *)
  | `CME        (* Chicago Mercantile Exchange *)
  | `CSFBALGO   (* CSFB Algos *)
  | `DRCTEDGE   (* Direct Edge *)
  | `DTB        (* EUREX *)
  | `ECBOT      (* CBOT *)
  | `EDGEA      (* Direct Edge *)
  | `EDGX       (* Bats EDGX Options Exchange *)
  | `ELX        (* ELX Futures *)
  | `FTA        (* Euronext NL Derivatives *)
  | `FWB        (* Frankfurt Stock Exchange *)
  | `GEMINI     (* ISE Gemini *)
  | `GLOBEX     (* CME GLOBEX *)
  | `HKFE       (* Hong Kong Futures Exchange *)
  | `HKMEX      (* Hong Kong Mercantile Exchange *)
  | `IBCFD
  | `IBIS       (* XETRA *)
  | `IBSX
  | `ICEUS      (* ICE Futures US *)
  | `IDEAL
  | `IDEALPRO
  | `IDEM       (* Borsa Italiana *)
  | `IEX        (* IEX New *)
  | `ISE        (* ISE Options Exchange *)
  | `ISLAND     (* INET *)
  | `JEFFALGO   (* Jefferies Auto-Trading Strategies *)
  | `KSE        (* Korea Stock Exchange *)
  | `LAVA       (* LavaFlow ECN *)
  | `LIFFE      (* NYSE Liffe *)
  | `LSE        (* London Stock Exchange *)
  | `MATIF      (* Euronext France *)
  | `MEFF       (* Spanish Futures & Options Exchange *)
  | `MERCURY    (* ISE Mercury *)
  | `MEXDER     (* Mexican Derivatives Exchange *)
  | `MEXI       (* Mexican Stock Exchange *)
  | `MIAX       (* MIAX Options Exchange *)
  | `MIBSX
  | `MONEP      (* Euronext France *)
  | `NASDAQ     (* NASDAQ *)
  | `NASDAQBX   (* NASDAQ BX *)
  | `NASDAQOM   (* NASDAQ OMX *)
  | `NFX        (* Nasdaq Futures Exchange *)
  | `NSX        (* National Stock Exchange *)
  | `NYBOT      (* New York Board of Trade *)
  | `NYMEX      (* New York Mercantile Exchange *)
  | `NYSE       (* New York Stock Exchange *)
  | `OMEGA      (* Omega ATS *)
  | `OMS        (* Stockholm Derivatives Exchange *)
  | `ONE        (* OneChicago *)
  | `PEARL      (* MIAX PEARL *)
  | `PHLX       (* Philadelphia Stock Exchange *)
  | `PINK       (* Pink OTC Markets *)
  | `PSE        (* Pacific Exchange *)
  | `PSX        (* NASDAQ OMX PSX *)
  | `PURE       (* Pure Trading *)
  | `SBF        (* Euronext France *)
  | `SBVM       (* Euronext Brussels Stocks *)
  | `SEHK       (* Hong Kong Stock Exchange *)
  | `SELECT     (* TMX Select *)
  | `SFB        (* Swedish Stock Exchange *)
  | `SGX        (* Singapore Exchange *)
  | `SMART      (* Smart Order Routing *)
  | `SNFE       (* Sydney Futures Exchange *)
  | `SOFFEX     (* EUREX *)
  | `SWB        (* Stuttgart Stock Exchange *)
  | `SWX        (* Swiss Exchange *)
  | `TGATE      (* Tradegate Exchange *)
  | `TPLUS2     (* T+2 Early Stock Settlement *)
  | `TRADEGATE  (* Tradegate Exchange *)
  | `TRQXCH     (* Turquoise CH *)
  | `TRQXDE     (* Turquoise DE *)
  | `TRQXEN     (* Turquoise *)
  | `TRQXEN     (* Turquoise *)
  | `TRQXEN     (* Turquoise *)
  | `TSE        (* Toronto Stock Exchange *)
  | `TSEJ       (* Tokyo Stock Exchange *)
  | `VENTURE    (* TSX Venture *)
  | `VIRTX      (* VIRT-X *)
  | `VSE        (* Vienna Stock Exchange *)
  | `VWAP       (* IB VWAP Dealing Network (VWAP) *)
  ] [@@deriving sexp, eq]
include Stringable.S with type t := t
include Twsable.S with type t := t
