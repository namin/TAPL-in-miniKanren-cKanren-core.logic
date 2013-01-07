(ns tapl.quines_nom
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp]))

(defn nomo [x]
  (predc x nom? `nom?))

(defn progo? [x]
  (not (some #{:closure} (flatten (list x)))))

(defn progo [x]
  (predc x progo? `progo?))

(defn lookupo [x env t]
  (fresh (y v rest)
         (conso [y v] rest env)
         (conde
          ((== y x) (== v t))
          ((!= y x) (lookupo x rest t)))))

(declare proper-listo)

(defn eval-expo [exp env val]
  (conde
   [(fresh (v)
      (== `(~'quote ~v) exp)
      (progo v)
      (== v val))]
   [(fresh (as)
      (conso 'list as exp)
      (progo as)
      (proper-listo as env val))]
    [(nomo exp)
      (lookupo exp env val)]
    [(fresh (rator rand body envc a)
       (nom/fresh [x]
         (== `(~rator ~rand) exp)
         (eval-expo rator env [:closure (nom/tie x body) envc])
         (eval-expo rand env a)
         (eval-expo body (lcons [x a] envc) val)))]
    [(fresh (body)
       (nom/fresh [x]
         (== `(~'fn ~(nom/tie x body)) exp)
         (== [:closure (nom/tie x body) env] val)))]))

(defn proper-listo [exp env val]
  (conde
   [(== '() exp)
    (== '() val)]
    [(fresh (a d t-a t-d)
      (conso a d exp)
      (conso t-a t-d val)
      (eval-expo a env t-a)
      (proper-listo d env t-d))]))

