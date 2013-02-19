(ns tapl.test.stlc_deriv
  (:use [tapl.stlc_deriv]
        clojure.test :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp]))

(deftest test-typingo
  (is (= (read-string
           (prn-str
             (run* [q]
               (fresh [c t d]
                 (nom/fresh [x]
                   (typingo-deriv ['typingo c () `(~'fn ~(nom/tie x x)) t] d)
                   (== q [c t d]))))))
        '([T-Abs [:=> _0 _0] ([[typingo T-Abs () (fn  [a_1] a_1) [:=> _0 _0]] <-- ([[typingo T-Var ([a_2 _0]) a_2 _0] <-- ()])])])))
  (is (= (run* [q]
           (fresh [c t d]
             (nom/fresh [x]
               (typingo-deriv ['typingo c () `(~'fn ~(nom/tie x [x x])) t] d))))
        ())))
