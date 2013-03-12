(ns tapl.test.quines_nom_red_big_base
  (:use [tapl.quines_nom_red_big_base] :reload)
  (:use [tapl.utils])
  (:use [clojure.test])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(deftest test-cljo
  (is (= (run* [q] (cljo '(quote quote) q)) '((quote quote))))
  (is (= (run* [q] (nom/fresh [a] (cljo (nom/tie a a) q))) '((fn [a_0] a_0))))
  (is (= (run* [q] (nom/fresh [a] (cljo `(~(nom/tie a a) (~'quote ~'quote)) q))) '(((fn [a_0] a_0) (quote quote))))))

(deftest test-quine-accept
  (let [p (first
            (run* [q]
              (fresh [p]
                (nom/fresh [a]
                  (== p `(~(nom/tie a `(~'list ~a (~'list (~'quote ~'quote) ~a)))
                           (~'quote ~(nom/tie a `(~'list ~a (~'list (~'quote ~'quote) ~a))))))
                  (evalo p `(~'quote ~p))
                  (cljo p q)))))]
    (is (= (eval p) (eval (eval p))))))

(deftest test-quine
  (let [p (first (run 1 [q] (fresh [p] (evalo p `(~'quote ~p)) (cljo p q))))]
    (is (= (eval p) (eval (eval p))))
    (is (= p '((fn [a_0] (list a_0 (list (quote quote) a_0)))
                (quote (fn [a_1] (list a_1 (list (quote quote) a_1)))))))))

(deftest test-quines
  (let [ps (run 10 [q] (fresh [p] (evalo p `(~'quote ~p)) (cljo p q)))]
    (doseq [p ps]
      (let [p (without-constraints p)]
        (is (= (eval (eval p)) (eval (eval (eval p)))))))))
