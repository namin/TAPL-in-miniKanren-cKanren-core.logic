(ns tapl.test.quines_nom_red_big
  (:use [tapl.quines_nom_red_big]
        :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]
        clojure.test :reload)
  (:require [clojure.pprint :as pp]))

(deftest test-quine-accept
  (let [r (run* [q]
            (nom/fresh [a]
              (== q `((~'fn ~(nom/tie a `(~'list ~a (~'list (~'quote ~'quote) ~a))))
                       (~'quote (~'fn ~(nom/tie a `(~'list ~a (~'list (~'quote ~'quote) ~a)))))))
              (evalo q q)))
        q (first (first r))
        p (read-string (prn-str q))]
    (is (= p (eval p)))))
