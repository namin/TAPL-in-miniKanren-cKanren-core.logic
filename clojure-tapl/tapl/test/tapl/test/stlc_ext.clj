(ns tapl.test.stlc_ext
  (:use [tapl.stlc_ext] :reload)
  (:use [clojure.test])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp]))

(deftest test-typingo
  (is (= (run* [q]
           (nom/fresh [x]
             (typingo () `(~'fn ~(nom/tie x x)) q)))
        '((:=> _0 _0))))
  (is (= (run* [q]
           (nom/fresh [x]
             (typingo () `(~'fn ~(nom/tie x [x x])) q)))
        ()))
  (is (= (run* [q]
           (typingo () `(~'if ~true ~false ~true) q))
        '(bool))))

(deftest test-redo*
  (is (= (run* [q]
           (nom/fresh [x]
             (redo* `(~'fn  ~(nom/tie x x)) q)))
        [`(~'fn ~(nom/tie 'a_0 'a_0))]))
  (is (= (run* [q]
           (nom/fresh [x]
             (redo* `(~'if ~true ~false ~true) q)))
        [false])))


(deftest test-compose-term
  (is (= (run* [q]
           (nom/fresh [f g a]
             (typingo () ['fn (nom/tie f ['fn (nom/tie g ['fn (nom/tie a [g [f a]])])])] q)))
        '([:=> [:=> _0 _1] [:=> [:=> _1 _2] [:=> _0 _2]]])))

  (is (= (run* [q]
           (nom/fresh [f g a]
             (typingo ()
               ['fn (nom/tie f ['fn (nom/tie g ['fn (nom/tie a [g [f a]])])])]
               [:=> [:=> 'A 'B]  [:=> [:=> 'B 'C] [:=> 'A 'C]]])))
        '(_0)))

  (is (= (run 1 [q]
           (typingo ()
             q
             [:=> [:=> 'A 'B]  [:=> [:=> 'B 'C] [:=> 'A 'C]]]))
        `((~'fn ~(nom/tie 'a_0 `(~'fn ~(nom/tie 'a_1 `(~'fn ~(nom/tie 'a_2 '(a_1 (a_0 a_2))))))))))))

