(ns tapl.test.quines_nom_red_small_debug
  (:use [tapl.quines_nom_red_small_debug]
        clojure.test :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp]))

(def omega [['fn (nom/tie 'a_0 '(a_0 a_0))] ['fn (nom/tie 'a_1 '(a_1 a_1))]])

(defn quine [a] `((~'fn ~(nom/tie a `(~'cons ~a (~'cons (~'cons (~'quote ~'quote) (~'cons ~a ~())) ~()))))
                   (~'quote (~'fn ~(nom/tie a `(~'cons ~a (~'cons (~'cons (~'quote ~'quote) (~'cons ~a ~())) ~())))))))

(deftest test-finite-positive-results
  (is (< 0 (count (run* [q] (fresh [o] (fresh [x] (quines-debug-so ['nomo x] 2 o) (== q [x o])))))))
  (is (< 0 (count (run* [q] (fresh [o] (fresh [e new a out] (quines-debug-so ['substo e new a out] 2 o) (== q [['substo e new a out] o])))))))
  (is (< 0 (count (run* [q] (fresh [o] (fresh [exp] (quines-debug-so ['valo exp] 2 o) (== q [exp o])))))))
  (is (< 0 (count (run* [q] (fresh [o] (fresh [exp v] (quines-debug-so ['valofo exp v] 2 o) (== q [['valofo exp v] o])))))))
  (is (< 0 (count (run* [q] (fresh [o] (fresh [exp out] (quines-debug-so ['redo exp out] 2 o) (== q [['redo exp out] o])))))))
  (is (< 0 (count (run* [q] (fresh [o] (fresh [e1 e2] (quines-debug-so ['redo* e1 e2] 2 o) (== q [['redo* e1 e2] o])))))))
  (is (< 0 (count (run* [q] (fresh [o] (fresh [e1 e2] (quines-debug-so ['qredo* e1 e2] 2 o) (== q [['qredo* e1 e2] o]))))))))

(deftest test-quine-accept
  (let [r (run* [q]
            (nom/fresh [a]
              (== q (quine a))
              (quines-debug-so ['redo* q q] 8 'no-overflow)))
        q (first r)
        p (read-string (prn-str q))]
    (is (= p (eval p)))))

(deftest test-omega-q
  (let [r (run 1 [q] (quines-debug-so ['qredo* q q] 5 'no-overflow))
        q (first r)]
    (is (= q omega))))

(deftest test-quine-accept-q
  (let [r (run* [q]
            (nom/fresh [a]
              (== q (quine a))
              (quines-debug-so ['qredo* q `(~'quote ~q)] 8 'no-overflow)))
        q (first r)
        p (read-string (prn-str q))]
    (is (= p (eval p)))))

(deftest test-no-improper-listo
  (is (= (run* [q]
           (quines-debug-so ['redo* `(~'cons (~'quote 1) (~'quote 2)) q] 100 'no-overflow))
        ())))

(deftest test-proper-listo
  (is (= (run* [q]
           (nom/fresh [a b]
             (quines-debug-so ['redo* `(~'cons (~'quote 1) ()) q] 100 'no-overflow)))
        '((1)))))

(deftest test-step
  (is (= (run* [q]
           (nom/fresh [a]
             (fresh [p]
               (quines-debug-so ['redo [['fn (nom/tie a ['cons a a])] ['cons ['quote 1] ()]] p] 100 'no-overflow)
               (quines-debug-so ['redo* p q] 100 'no-overflow))))
         '(((1) 1)))))
