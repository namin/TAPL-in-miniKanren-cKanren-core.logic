# tapl

Translation of Benjamin C. Pierce's excellent 'Types and Programming Languages' (MIT Press, 2002) into miniKanren/cKanren (R6RS Scheme) and core.logic (Clojure).  This is the Clojure version, David Nolen's using core.logic and core.match libraries.

This is an exercise to help me learn more about types, better understand the strengths and limitations of miniKanren, decide which constraints should be added to cKanren, and learn Clojure and core.logic

Thanks to Dan Friedman for suggesting this exercise years ago.

--Will

## Usage

From the 'tapl' directory, use Leiningen to get the dependencies (you have installed Leiningen, right?):

    lein deps

To run the tests, just type:

    lein test

To play with the code in a REPL, type this in your terminal:

    lein REPL

which should give you something like this:

    Williams-MacBook-Air:tapl webyrd$ lein repl
    REPL started; server listening on localhost port 3224
    user=>

and then import tapl, core.logic, and core.match by tying (or pasting) this at the REPL:

    (ns tapl-repl
      (:use [tapl.core])
      (:refer-clojure :exclude [==])
      (:use [clojure.core.logic])
      (:use [clojure.core.match :only [match]])
      (:require [clojure.pprint :as pp]))

Now you should be ready to test the tapl code:

    tapl-repl=> (run 10 [q] (T q))
    (:true :false :zero [:succ :true] [:succ :false] [:if :true :true :true] [:succ :zero] [:if :true :true :false] [:pred :true] [:if     :true :true :zero])
    tapl-repl=>

## Installing a snapshot of core.logic

If Leiningen gives you the error _"It's possible the specified jar is
not in any repository."_, you need to install a snapshot release of
'core.logic'.

This project relies on the master version of core.logic. Install it by
downloading the sources somewhere:

    git clone git://github.com/clojure/core.logic.git

Then, from the 'core.logic' directory, run

    lein test; lein install

The _nom_ files use nominal unification, only available as a prototype
in the
[nominal-base](https://github.com/namin/core.logic/compare/master...nominal-base)
branch of [namin/core.logic](https://github.com/namin/core.logic).

    (ns quine-repl
      (:use [tapl.quines_nom_red_small])
      (:refer-clojure :exclude [==])
      (:use [clojure.core.logic :exclude [is] :as l]
            [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
      (:require [clojure.pprint :as pp]))

## License

Copyright (C) 2012 William E. Byrd
Copyright (C) 2012 Nada Amin

Distributed under the Eclipse Public License, the same as Clojure.
