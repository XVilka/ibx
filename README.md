IBX - OCaml implementation of the Interactive Brokers TWS API
==================================================================

---------------------------------------------------------------------------

IBX is a pure OCaml implementation of the
[Interactive Brokers](http://www.interactivebrokers.com/en/main.php)
Trader Workstation API (TWS API) built on top of Jane Street's
[Core](https://github.com/janestreet/core) and
[Async](https://github.com/janestreet/async) library.

__DISCLAIMER:__ This software is not approved by Interactive Brokers or any
of its affiliates. It comes with absolutely no warranty and the use of
this software for actual trading is at your own risk.

Installation
------------

From [OPAM](http://opam.ocaml.org)

    $ opam install ibx

From Source

    $ make
    $ make install

Usage
-----

### Documentation

The _still incomplete_ API-documentation of this distribution can be built with `make doc`.
It can also be found [online](http://ogu.bitbucket.io/ibx/api/).

### Prerequisites

Before you start, please install the Interactive Brokers Trader Workstation
[TWS](http://www.interactivebrokers.com/en/p.php?f=tws) or its low-resource alternative
[IB Gateway](https://www.interactivebrokers.com/en/?f=%2Fen%2Fcontrol%2Fsystemstandalone-ibGateway.php%3Fos%3Dunix)
and make sure that the software allows for incoming API connections. How this is
done for TWS can be watched [here](http://www.youtube.com/watch?v=53tmypRq5wI).
For more detailed information please refer to the Interactive Brokers
[API Reference Guide](http://www.interactivebrokers.com/en/software/api/api.htm).

__NOTE:__ You can use the username _edemo_ with password _demouser_
to log into the demo account.

### Examples

This simple program will connect to the IB Gateway running on localhost
and retrieve the closing price for a given stock symbol:

    :::ocaml
    open Core
    open Async
    open Ibx

    let host = "localhost"
    let port = 4001

    let () =
      Command.async
        ~summary:"Show the closing price of the given stock symbol"
        Command.Spec.(
          empty
          +> flag "-currency" ~doc:" currency of the stock price"
            (optional_with_default `USD (Arg_type.create Currency.of_string))
          +> anon ("STOCK-SYMBOL" %: Arg_type.create Symbol.of_string)
        )
        (fun currency symbol () ->
          Tws.with_client ~host ~port ~on_handler_error:(`Call (fun e ->
            eprintf "[Error] Failed to retrieve closing price for %s: %s\n%!"
              (Symbol.to_string symbol) (Error.to_string_hum e);
          ))
          (fun tws ->
            Tws.latest_close_exn tws ~contract:(Contract.stock symbol ~currency)
            >>= fun close ->
            printf "[Info] Closing price for %s is %4.2f %s\n"
              (Symbol.to_string symbol) (Close.price close |> Price.to_float)
              (Currency.to_string currency);
            return ()
          )
        )
      |> Command.run


Assuming the above program is stored in the file `show_close.ml`,
you can simply build it by running

    $ ocamlbuild -use-ocamlfind -tag thread -pkg ibx show_close.native

and then use it to get the closing price of a stock (e.g. Apple Inc.)
as follows:

    $ ./show_close.native AAPL

For more complex examples please refer to the `examples`-directory of this
distribution. For instance, you can build the `plot_history` example by typing

    $ ocamlbuild -use-ocamlfind -tag thread -pkg ibx,gnuplot plot_history.native

and then run

    $ ./plot_history.native -sma 100 AAPL

to plot a candle stick chart of Apple prices with a simple 100-day moving average:

![AAPL SMA](http://ogu.bitbucket.io/aapl_sma.png)

Moreover, to plot 1 minute of intraday trade and quote (TAQ) data, type

    $ ocamlbuild -use-ocamlfind -tag thread -pkg ibx,gnuplot plot_taq_data.native
    $ ./plot_taq_data.native AAPL

which creates a plot similar to this image

![AAPL TAQ](http://ogu.bitbucket.io/aapl_taq.png)


__NOTE:__ This TAQ data plots does not reflect the true market history, since
Interactive Brokers accumulates market data in time increments of 300 milliseconds.

### More Examples

For even more examples, please check out the
[IBX command line tools](https://bitbucket.org/ogu/ibx-tools).


Contact Information
-------------------

In case of bugs, feature requests and similar, please contact:

  * Oliver Gu <gu.oliver@yahoo.com>
