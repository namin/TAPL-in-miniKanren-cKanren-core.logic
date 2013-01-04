(ns tapl.test.quines_nom_env
  (:use [tapl.quines_nom_env]
        clojure.test :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp]))

(deftest test-quine
  (let [p (first (run 1 [q] (nom/fresh [closure-nom] (eval-expo closure-nom q '() q))))
        p (read-string (prn-str p))]
    (is (= p (eval p)))
    (is (= p '((fn [a_0] (list a_0 (list (quote quote) a_0)))
                (quote (fn [a_0] (list a_0 (list (quote quote) a_0)))))))))

(deftest test-twine
  (let [[x y] (first
                (run 1 [q]
                  (fresh [x y]
                    (nom/fresh [closure-nom]
                      (!= x y)
                      (eval-expo closure-nom x '() y)
                      (eval-expo closure-nom y '() x)
                      (== [x y] q)))))
        x (read-string (prn-str x))
        y (read-string (prn-str y))]
    (is (not (= x y)))
    (is (= (eval x) y))
    (is (= (eval y) x))))
