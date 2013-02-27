(ns tapl.test.quines_nom_red_big_up
  (:use [tapl.quines_nom_red_big_up] :reload)
  (:use [tapl.utils])
  (:use [clojure.test])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp]))

(deftest test-check-ties-o
  (is (= (run* [q] (check-ties-o q)) '((_0 :- (check-ties _0 nil)))))
  (is (= (run* [q] (check-ties-o q) (== 1 q)) '(1)))
  (is (= (run* [q] (nom/fresh [a] (check-ties-o q) (== (nom/tie a a) q))) '()))
  (is (= (run* [q] (nom/fresh [a] (check-ties-o q) (== '(1) q))) '((1))))
  (is (= (run* [q] (nom/fresh [a] (check-ties-o q) (== `(~'fn ~(nom/tie a a)) q)))
        `((~'fn ~(nom/tie 'a_0 'a_0)))))
  (is (= (run* [q] (nom/fresh [a] (check-ties-o q) (== `(~'foo ~(nom/tie a a)) q)))
        '()))
  (is (= (run* [q] (nom/fresh [a] (check-ties-o q) (== `(~'fn ~(nom/tie a (nom/tie a a))) q)))
        '())))


(deftest test-quine-accept
  (let [r (run* [q]
            (nom/fresh [a]
              (== q `((~'fn ~(nom/tie a `(~'list ~a (~'list (~'quote ~'quote) ~a))))
                       (~'quote (~'fn ~(nom/tie a `(~'list ~a (~'list (~'quote ~'quote) ~a)))))))
              (evalo q `(~'quote ~q))))
        q (first r)
        p (read-string (prn-str q))]
    (is (= p (eval p)))))

(deftest test-quine
  (let [p (first (run 1 [q] (evalo q `(~'quote ~q))))
        p (read-string (prn-str p))]
    (is (= p (eval p)))
    (is (= p '((fn [a_0] (list a_0 (list (quote quote) a_0)))
                (quote (fn [a_0] (list a_0 (list (quote quote) a_0)))))))))

(deftest test-quines
  (let [ps (run 10 [q] (evalo q `(~'quote ~q)))]
    (doseq [p ps]
      (let [p (read-string (prn-str (without-constraints p)))]
        (is (= (eval p) (eval (eval p))))))))

(deftest test-regression-1
  (= (read-string (prn-str (run* [q] (nom/fresh [a b] (evalo `((~'fn ~(nom/tie a `(~'fn ~(nom/tie b `(~a ~b))))) (~'fn ~(nom/tie a a))) q)))))
    '((fn [a_0] ((fn [a_1] a_1) a_0)))))
