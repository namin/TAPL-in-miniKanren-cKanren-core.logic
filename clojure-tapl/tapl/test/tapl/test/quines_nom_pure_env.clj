(ns tapl.test.quines_nom_pure_env
  (:use [tapl.quines_nom_pure_env]  :reload)
  (:use [clojure.test])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(deftest test-quine
  (let [p (first (run 1 [q] (fresh [p] (nom/fresh [closure-nom] (eval-expo closure-nom p '() p)) (cljo p q))))]
    (is (= (eval p) (eval (eval p))))))

(deftest test-quines
  (let [ps (run 10 [q] (fresh [p] (nom/fresh [closure-nom] (eval-expo closure-nom p '() p)) (cljo p q)))]
    (doseq [p ps]
      (is (= (eval p) (eval (eval p)))))))

(deftest test-env-ok
  (is (= (run* [q]
           (fresh [exp env val]
             (nom/fresh [clojure-nom a]
               (== exp (nom/tie a a))
               (eval-expo clojure-nom exp env val)
               (== env q))))
        '(_0))))
