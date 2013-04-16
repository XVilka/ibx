(* File: exchange.mli

   IBX - OCaml implementation of the Interactive Brokers TWS API

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
open Tws_prot

(* Futher information about the exchange listings can be found
   {{:http://www.interactivebrokers.com/en/index.php?f=exchanges} online}. *)

type t =
[ `SMART      (* Smart Order Routing *)
| `ARCAEDGE   (* ArcaEdge *)
| `BYX        (* BATS BYX *)
| `BATS       (* BATS Global Markets *)
| `BTRADE     (* Bloomberg Tradebook *)
| `BOX        (* Boston Options Exchange *)
| `CFE        (* CBOE Futures Exchange *)
| `CBSX       (* CBOE Stock Exchange *)
| `ECBOT      (* CBOT *)
| `CBOE       (* Chicago Board Options Exchange *)
| `CBOE2      (* CBOE C2 *)
| `CHX        (* Chicago Stock Exchange *)
| `GLOBEX     (* CME GLOBEX *)
| `DRCTEDGE   (* Direct Edge *)
| `EDGEA      (* Direct Edge *)
| `ELX        (* ELX Futures *)
| `ICEUS      (* ICE Futures US *)
| `ISLAND     (* INET *)
| `ISE        (* ISE Options Exchange *)
| `LAVA       (* LavaFlow ECN *)
| `NASDAQ     (* NASDAQ *)
| `NFX        (* Nasdaq Futures Exchange *)
| `NASDAQOM   (* NASDAQ OMX *)
| `BEX        (* NASDAQ OMX BX *)
| `PSX        (* NASDAQ OMX PSX *)
| `NSX        (* National Stock Exchange *)
| `NYBOT      (* New York Board of Trade *)
| `NYMEX      (* New York Mercantile Exchange *)
| `NYSE       (* New York Stock Exchange *)
| `AMEX       (* NYSE AMEX *)
| `ARCA       (* NYSE Arca *)
| `LIFFE      (* NYSE Liffe *)
| `ONE        (* OneChicago *)
| `PSE        (* Pacific Exchange *)
| `PHLX       (* Philadelphia Stock Exchange *)
| `PINK       (* Pink OTC Markets *)
| `ALPHA      (* Alpha ATS *)
| `CDE        (* Montreal Exchange *)
| `OMEGA      (* Omega ATS *)
| `PURE       (* Pure Trading *)
| `SELECT     (* TMX Select *)
| `TSE        (* Toronto Stock Exchange *)
| `VENTURE    (* TSX Venture *)
| `MEXDER     (* Mexican Derivatives Exchange *)
| `MEXI       (* Mexican Stock Exchange *)
| `VSE        (* Vienna Stock Exchange *)
| `BATEEN     (* BATS Europe *)
| `CHIXEN     (* CHI-X Europe LTD Clearnet *)
| `SBVM       (* Euronext Brussels Stocks *)
| `TRQXEN     (* Turquoise *)
| `CHIXEN     (* CHI-X Europe LTD Clearnet *)
| `MATIF      (* Euronext France *)
| `MONEP      (* Euronext France *)
| `SBF        (* Euronext France *)
| `TRQXEN     (* Turquoise *)
| `BATEDE     (* BATS Europe *)
| `CHIXDE     (* CHI-X Europe LTD Clearstream *)
| `DTB        (* EUREX *)
| `FWB        (* Frankfurt Stock Exchange *)
| `SWB        (* Stuttgart Stock Exchange *)
| `TRADEGATE  (* Tradegate Exchange *)
| `TRQXDE     (* Turquoise DE *)
| `IBIS       (* XETRA *)
| `BVME       (* Borsa Italiana *)
| `IDEM       (* Borsa Italiana *)
| `IBCFD
| `FTA        (* Euronext NL Derivatives *)
| `AEB        (* Euronext NL Stocks *)
| `TRQXEN     (* Turquoise *)
| `BM         (* Bolsa de Madrid *)
| `MEFF       (* Spanish Futures & Options Exchange *)
| `OMS        (* Stockholm Derivatives Exchange *)
| `SFB        (* Swedish Stock Exchange *)
| `BATECH     (* BATS Europe *)
| `CHIXCH     (* CHI-X Europe Ltd Swiss *)
| `SOFFEX     (* EUREX *)
| `SWX        (* Swiss Exchange *)
| `TRQXCH     (* Turquoise CH *)
| `VIRTX      (* VIRT-X *)
| `BATEUK     (* BATS Europe *)
| `CHIXUK     (* CHI-X Europe Ltd Crest *)
| `LSE        (* London Stock Exchange *)
| `ASX        (* Australian Stock Exchange *)
| `SNFE       (* Sydney Futures Exchange *)
| `HKFE       (* Hong Kong Futures Exchange *)
| `HKMEX      (* Hong Kong Mercantile Exchange *)
| `SEHK       (* Hong Kong Stock Exchange *)
| `IBSX
| `CHIXJ      (* CHI-X Japan *)
| `TSEJ       (* Tokyo Stock Exchange *)
| `SGX        (* Singapore Exchange *)
| `KSE        (* Korea Stock Exchange *)
| `IDEAL
| `IDEALPRO
| `MIBSX
] with sexp
include Stringable.S with type t := t
include Twsable.S with type t := t
