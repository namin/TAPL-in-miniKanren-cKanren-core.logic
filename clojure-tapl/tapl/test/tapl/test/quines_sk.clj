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
  (is (= (run* [q] (fresh [in2 out1] (evalo '(. h) in2 out1 ()) (== q [in2 out1])))
        '([(i) (h)])))
  (is (= (run* [q] (fresh [in2 out1 out2] (evalo '(_ i . h) in2 out1 ()) (== q [in2 out1])))
        '([(i) (h)])))
  (is (= (run* [q] (fresh [in2 out1] (evalo '(_ . h . e) in2 out1 ()) (== q [in2 out1])))
        '([(i) (h e)]))))

(deftest test-hello
  (is (= (map #(apply str %)
           (run* [q] (evalo sk-hello '(i) q ())))
        '("Hello world"))))

