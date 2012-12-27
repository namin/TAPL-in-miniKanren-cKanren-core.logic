(ns tapl.test.quines_nom_red_small
  (:use [tapl.quines_nom_red_small]
        :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]
        clojure.test :reload)
  (:require [clojure.pprint :as pp]))

(def omega [['fn (nom/tie 'a_0 '(a_0 a_0))] ['fn (nom/tie 'a_1 '(a_1 a_1))]])

(defn quine [a] `((~'fn ~(nom/tie a `(~'cons ~a (~'cons (~'cons (~'quote ~'quote) (~'cons ~a ~'nil)) ~'nil))))
                   (~'quote (~'fn ~(nom/tie a `(~'cons ~a (~'cons (~'cons (~'quote ~'quote) (~'cons ~a ~'nil)) ~'nil)))))))

(deftest test-quine-accept
  (let [r (run* [q]
            (nom/fresh [a]
              (== q (quine a))
              (redo* q q)))
        q (first (first r))
        p (read-string (prn-str q))]
    (is (= p (eval p)))))

(deftest test-omega-q
  (let [r (run 1 [q] (qredo* q q))
        q (first r)]
    (is (= q omega))))

(deftest test-quine-accept-q
  (let [r (run* [q]
            (nom/fresh [a]
              (== q (quine a))
              (qredo* q `(~'quote ~q))))
        q (first (first r))
        p (read-string (prn-str q))]
    (is (= p (eval p)))))
