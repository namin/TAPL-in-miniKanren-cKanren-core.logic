(ns tapl.test.stlc
  (:use [tapl.stlc]
        clojure.test :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp]))

(deftest test-typingo
  (is (= (run* [q]
           (nom/fresh [x]
             (typingo () [:lam (nom/tie x x)] q)))
        '((:=> _0 _0))))
  (is (= (run* [q]
           (nom/fresh [x]
             (typingo () [:lam (nom/tie x [x x])] q)))
        ())))

(deftest test-redo*
  (is (= (run* [q]
           (nom/fresh [x]
             (redo* [:lam (nom/tie x x)] q)))
        [[:lam (nom/tie 'a_0 'a_0)]])))


(deftest test-compose-term
  (is (= (run* [q]
           (nom/fresh [f g a]
             (typingo () [:lam (nom/tie f [:lam (nom/tie g [:lam (nom/tie a [g [f a]])])])] q)))
        '([:=> [:=> _0 _1] [:=> [:=> _1 _2] [:=> _0 _2]]])))

  (is (= (run* [q]
           (nom/fresh [f g a]
             (typingo ()
               [:lam (nom/tie f [:lam (nom/tie g [:lam (nom/tie a [g [f a]])])])]
               [:=> [:=> 'A 'B]  [:=> [:=> 'B 'C] [:=> 'A 'C]]])))
        '(_0)))

  (is (= (run 1 [q]
           (typingo ()
             q
             [:=> [:=> 'A 'B]  [:=> [:=> 'B 'C] [:=> 'A 'C]]]))
        [[:lam (nom/tie 'a_0 [:lam (nom/tie 'a_1 [:lam (nom/tie 'a_2 '[a_1 [a_0 a_2]])])])]])))

(deftest test-generation
  (let [r (run 10 [q]
            (fresh [t]
              (typingo () q [:=> 'A 'A])
              (redo q t)))
         r (read-string (prn-str r))]
    (is (= r
          '([[:lam  [a_0] a_0] [:lam  [a_1] a_1]]
            [[:lam  [a_0] [:lam  [a_1] a_1]] [:lam  [a_2] a_2]]
            [[[:lam  [a_0] a_0] [:lam  [a_1] a_1]] [:lam  [a_2] a_2]]
            [[:lam  [a_0] [:lam  [a_1] a_1]] [:lam  [a_2] [:lam  [a_3] a_3]]]
            [[:lam  [a_0] [[:lam  [a_1] a_1] a_0]] [:lam  [a_2] a_2]]
            [[[:lam  [a_0] [:lam  [a_1] a_1]] [:lam  [a_2] a_2]] [:lam  [a_3] a_3]]
            [[:lam  [a_0] [a_0 [:lam  [a_1] a_1]]] [:lam  [a_2] a_2]]
            [[:lam  [a_0] [:lam  [a_1] a_1]] [:lam  [a_2] [:lam  [a_3] a_2]]]
            [[:lam  [a_0] [:lam  [a_1] [[:lam  [a_2] a_2] a_1]]] [:lam  [a_3] a_3]]
            [[[:lam  [a_0] a_0] [:lam  [a_1] [:lam  [a_2] a_2]]] [:lam  [a_3] a_3]])))))
