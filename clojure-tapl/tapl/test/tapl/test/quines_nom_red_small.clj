(ns tapl.test.quines_nom_red_small
  (:use [tapl.quines_nom_red_small]
        clojure.test :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp]))

(def omega [['fn (nom/tie 'a_0 '(a_0 a_0))] ['fn (nom/tie 'a_1 '(a_1 a_1))]])

(defn quine [a] `((~'fn ~(nom/tie a `(~'cons ~a (~'cons (~'cons (~'quote ~'quote) (~'cons ~a ~())) ~()))))
                   (~'quote (~'fn ~(nom/tie a `(~'cons ~a (~'cons (~'cons (~'quote ~'quote) (~'cons ~a ~())) ~())))))))

(deftest test-quine-accept
  (let [r (run* [q]
            (nom/fresh [a]
              (== q (quine a))
              (redo* q q)))
        q (first r)
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
        q (first r)
        p (read-string (prn-str q))]
    (is (= p (eval p)))))

(deftest test-no-listo-looping
  (let [r (run 4 [q]
            (fresh [p1 p2]
              (nom/fresh [a b]
                (== p2 `((~'fn ~(nom/tie a a)) (~'fn ~(nom/tie b b))))
                (redo* p1 p2)
                (== q [p1 p2]))))]
    (doseq [q r]
      (let [p1 (read-string (prn-str (first q)))
            p2 (read-string (prn-str (second q)))]
        (is (= (eval p1) p2))))))

(deftest test-no-improper-listo
  (is (= (run* [q]
           (redo* `(~'cons (~'quote 1) (~'quote 2)) q))
        ())))

(deftest test-proper-listo
  (is (= (run* [q]
           (nom/fresh [a b]
             (redo* `(~'cons (~'quote 1) ()) q)))
        '((1)))))
