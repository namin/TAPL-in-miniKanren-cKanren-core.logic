(ns tapl.test.quines_nom_env
  (:use [tapl.quines_nom_env]
        :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]
        clojure.test :reload)
  (:require [clojure.pprint :as pp]))

(deftest test-quine
  (let [p (first (first (run 1 [q] (nom/fresh [closure-nom] (eval-expo closure-nom q '() q)))))
        p (read-string (prn-str p))]
    (is (= p (eval p)))
    (is (= p '((fn [a_0] (list a_0 (list (quote quote) a_0)))
                (quote (fn [a_0] (list a_0 (list (quote quote) a_0)))))))))
