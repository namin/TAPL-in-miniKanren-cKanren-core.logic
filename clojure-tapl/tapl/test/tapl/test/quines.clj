(ns tapl.test.quines
  (:use [tapl.quines])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is]])
  (:use [clojure.core.match :only [match]])
  (:use [clojure.test]))

(deftest test-symbolo
  (is (= (run* [q] (symbolo q)) '((_.0 :- clojure.core/symbol?))))
  (is (= (run* [q] (symbolo q) (== 'foo q)) '(foo)))
  (is (= (run* [q] (symbolo q) (== 5 q)) '()))
  (is (= (run* [q] (symbolo q) (== '(hello there) q)) '())))

(deftest test-not-in-envo
  (is (= (run* [q] (not-in-envo q '())) '(_.0)))
  (is (= (run* [q] (not-in-envo 'x '([y v]))) '(_.0)))
  (is (= (run* [q] (not-in-envo 'x '([x v]))) '()))
  (is (= (run* [q] (not-in-envo 'x '([y v] [x v]))) '())))

(deftest test-lookupo
  (is (= (run* [q] (lookupo 'x '() q)) '()))
  (is (= (run* [q] (lookupo 'x '([y v]) q)) '()))
  (is (= (run* [q] (lookupo 'x '([x v]) q)) '(v)))
  (is (= (run* [q] (lookupo 'x '([y vy] [x vx]) q)) '(vx))))

(deftest test-eval-expo
  (is (= (run* [q] (eval-expo '(quote v) '() q)) '(v)))
  (is (= (run* [q] (eval-expo '(list x) '([x v]) q)) '((v))))
  (is (= (run* [q] (eval-expo '((fn [x] x) (quote v)) '() q)) '(v))))

(deftest test-quine
  (is (= (map first (run 1 [q] (eval-expo q '() q)))
         '(((fn [_.0] (list _.0 (list 'quote _.0)))
            '(fn [_.0] (list _.0 (list 'quote _.0))))))))

(deftest test-twine
  (is (= (map first (run 1 [q] (fresh [x y]
                                      (!= x y)
                                      (eval-expo x '() y)
                                      (eval-expo y '() x)
                                      (== [x y] q))))
         '(['((fn [_.0] (list 'quote (list _.0 (list 'quote _.0))))
              '(fn [_.0] (list 'quote (list _.0 (list 'quote _.0)))))
            ((fn [_.0] (list 'quote (list _.0 (list 'quote _.0))))
             '(fn [_.0] (list 'quote (list _.0 (list 'quote _.0)))))]))))
