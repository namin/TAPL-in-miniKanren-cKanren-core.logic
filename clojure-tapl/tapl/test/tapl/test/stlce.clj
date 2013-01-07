(ns tapl.test.stlce
  (:use [tapl.stlce]
        clojure.test :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp]))

(deftest test-typingo
  (is (= (run* [q]
           (nom/fresh [x]
             (typingo () `(~'fn ~(nom/tie x x)) q)))
        '((type (:=> _0 _0)))))
  (is (= (run* [q]
           (nom/fresh [x]
             (typingo () `(~'fn ~(nom/tie x [x x])) `(~'type ~q))))
        ()))
  (is (= (run* [q]
           (nom/fresh [x]
             (typingo () `(~'fn ~(nom/tie x [x x])) q)))
        '(((error (not-arrow-type _0)) :- (!= (_0 [:=> _1 _2])))
          ;; TODO(namin): The != below is spurious b/c of occurs-check. Is it worth fixing?
          ((error (incompatible-types _0 [:=> _0 _1])) :- (!= (_0 [:=> _0 _1])))))))

(deftest test-typingo-unbound-var
  (is (= (run* [q]
           (nom/fresh [x]
             (typingo () x q)))
        '((error (unbound-var)))))
  (is (= (run* [q]
           (nom/fresh [x y]
             (typingo () `(~'fn ~(nom/tie x y)) q)))
        '((error (unbound-var))))))

(deftest test-compose-term
  (is (= (run 1 [q]
           (typingo ()
             q
             `(~'type [:=> [:=> 'A 'B]  [:=> [:=> 'B 'C] [:=> 'A 'C]]])))
        `((~'fn ~(nom/tie 'a_0 `(~'fn ~(nom/tie 'a_1 `(~'fn ~(nom/tie 'a_2 '(a_1 (a_0 a_2))))))))))))
