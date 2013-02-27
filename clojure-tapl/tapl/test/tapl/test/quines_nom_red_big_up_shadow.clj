(ns tapl.test.quines_nom_red_big_up_shadow
  (:use [tapl.quines_nom_red_big_up_shadow] :reload)
  (:use [tapl.utils])
  (:use [clojure.test])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp]))

(deftest test-check-ties-o
  (is (= (run* [q] (nom/fresh [fn-nom] (check-ties-o fn-nom q))) '((_0 :- (check-ties _0 nil)))))
  (is (= (run* [q] (nom/fresh [fn-nom] (check-ties-o fn-nom q) (== 1 q))) '(1)))
  (is (= (run* [q] (nom/fresh [fn-nom a] (check-ties-o fn-nom q) (== (nom/tie a a) q))) '()))
  (is (= (run* [q] (nom/fresh [fn-nom a] (check-ties-o fn-nom q) (== '(1) q))) '((1))))
  (is (= (run* [q] (nom/fresh [fn-nom a] (check-ties-o fn-nom q) (== `(~fn-nom ~(nom/tie a a)) q)))
        `((~'a_0 ~(nom/tie 'a_1 'a_1)))))
  (is (= (run* [q] (nom/fresh [fn-nom a] (check-ties-o fn-nom q) (== `(~'foo ~(nom/tie a a)) q)))
        '()))
  (is (= (run* [q] (nom/fresh [fn-nom a] (check-ties-o fn-nom q) (== `(~a ~(nom/tie a a)) q)))
        '()))
  (is (= (run* [q] (nom/fresh [fn-nom a] (check-ties-o fn-nom q) (== `(~fn-nom ~(nom/tie a (nom/tie a a))) q)))
        '())))

(deftest test-clojurifyo
  (is (= (run* [x]
           (fresh [p q noms]
             (nom/fresh [fn-nom list-nom quote-nom]
               (== noms [fn-nom list-nom quote-nom])
               (nom/fresh [a]
                 (== p `((~fn-nom ~(nom/tie a `(~list-nom ~a (~list-nom (~quote-nom ~quote-nom) ~a))))
                          (~quote-nom (~fn-nom ~(nom/tie a `(~list-nom ~a (~list-nom (~quote-nom ~quote-nom) ~a)))))))
                 (== q `((~'fn ~(nom/tie a `(~'list ~a (~'list (~'quote ~'quote) ~a))))
                          (~'quote (~'fn ~(nom/tie a `(~'list ~a (~'list (~'quote ~'quote) ~a)))))))
                 (clojurifyo noms p q)))))
        '(_0))))

(deftest test-quine-accept
  (let [r (run* [q]
            (fresh [p noms]
              (nom/fresh [fn-nom list-nom quote-nom]
                (== noms [fn-nom list-nom quote-nom])
                (nom/fresh [a]
                  (== p `((~fn-nom ~(nom/tie a `(~list-nom ~a (~list-nom (~quote-nom ~quote-nom) ~a))))
                           (~quote-nom (~fn-nom ~(nom/tie a `(~list-nom ~a (~list-nom (~quote-nom ~quote-nom) ~a)))))))
                  (clojurifyo noms p q)
                  (evalo noms p `(~quote-nom ~p))))))
        q (first r)
        p (read-string (prn-str q))]
    (is (= (eval p) (eval (eval p))))))

(deftest test-quine
  (let [p (first
            (run 1 [q]
              (fresh [p noms]
                (nom/fresh [fn-nom list-nom quote-nom]
                  (== noms [fn-nom list-nom quote-nom])
                  (evalo noms p `(~quote-nom ~p))
                  (clojurifyo noms p q)))))
        p (read-string (prn-str p))]
    (is (= (eval p) (eval (eval p))))
    (is (= p '((fn [a_0] (list a_0 (list (quote quote) a_0)))
                (quote (fn [a_1] (list a_1 (list (quote quote) a_1)))))))))

(deftest test-quines
  (let [ps (run 10 [q]
             (fresh [p noms]
               (nom/fresh [fn-nom list-nom quote-nom]
                 (== noms [fn-nom list-nom quote-nom])
                 (evalo noms p `(~quote-nom ~p))
                 (clojurifyo noms p q))))]
    (doseq [p ps]
      (let [p (read-string (prn-str (without-constraints p)))]
        (is (= (eval p) (eval (eval p))))))))

(deftest test-shadow
  (is (= (run 1 [q]
           (fresh [p noms]
             (nom/fresh [fn-nom list-nom quote-nom]
               (== noms [fn-nom list-nom quote-nom])
               (evalo noms `((~fn-nom ~(nom/tie fn-nom `(~fn-nom (~quote-nom ~quote-nom)))) (~fn-nom ~(nom/tie list-nom list-nom))) p)
               (clojurifyo noms p q))))
        '((quote quote)))))
