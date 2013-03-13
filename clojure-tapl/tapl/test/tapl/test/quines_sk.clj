(ns tapl.test.quines_sk
  (:use [tapl.quines_sk] :reload)
  (:use [tapl.utils])
  (:use [clojure.test])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(deftest test-conversion-sanity
  (is (= unl-quine (sk2unl (unl2sk unl-quine)))))

(deftest test-basics
  (is (= (run* [q] (fresh [in2 out1 out2] (evalo '(_ . h i) in2 out1 ()) (== q [in2 out1])))
        '([(i) (h)])))
  (is (= (run* [q] (fresh [in2 out1] (evalo '(_ _ . h . e i) in2 out1 ()) (== q [in2 out1])))
        '([(i) (h e)]))))

(deftest test-hello
  (is (= (map #(apply str %)
           (run* [q] (evalo sk-hello '(i) q ())))
        '("Hello world"))))

(deftest test-sk
  (is (= (run* [q] (fresh [in2 out1] (evalo '(_ _ _ s k s i) in2 out1 ()) (== q [in2 out1])))
        '([(i) ()])))
  (is (= (run* [q] (fresh [in2 out1] (evalo '(_ _ k i s) in2 out1 ()) (== q [in2 out1])))
        '([(i) ()])))
  )

(deftest test-iter
  (is (= (run* [q] (fresh [in2 out1] (evalo (unl2sk "```si`k``s.*``sii``si``si``si``si``si``si``si``si`ki") in2 out1 ()) (== q [in2 out1])))
        '([(i) (* * * * * * * *)])))
  (is (= (run* [q] (fresh [in2 out1] (evalo (unl2sk "```si`k``s.*``sii``si``si`ki") in2 out1 ()) (== q [in2 out1])))
        '([(i) (* *)]))))

(deftest test-hello-n
  (is (= (map #(apply str %)
           (run* [q] (fresh [i o1 o2] (evalo sk-hello-n '(i) q ()))))
        '("Hello world!Hello world!Hello world!Hello world!Hello world!Hello world!Hello world!Hello world!"))))

