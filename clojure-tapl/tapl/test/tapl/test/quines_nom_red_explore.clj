(ns tapl.test.quines_nom_red_explore
  (:use [tapl.quines_nom_red_explore]
        [tapl.quines_nom_red_big_base]
    :reload)
  (:use [tapl.utils])
  (:use [clojure.test])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
    [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(deftest test-redfo*
  (is (= (run* [q] (fresh [p] (redfo* `(~'list (~'quote ~'a) (~'list (~'quote ~'b) (~'quote ~'c))) p) (cljo p q)))
        '((quote (a (b c)))))))

(deftest test-quine
  (let [p (first (run 1 [q] (fresh [p] (redfo* p `(~'quote ~p)) (cljo p q))))]
    (is (= (eval p) (eval (eval p))))))
