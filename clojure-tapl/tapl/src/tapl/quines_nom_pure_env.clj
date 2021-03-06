(ns tapl.quines_nom_pure_env
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(defn lookupo [x env t]
  (all
    (fresh [ignore]
      (nom/hash x ignore))
    (fresh [y v rest]
      (conso [y v] rest env)
      (conde
        ((== y x) (== v t))
        ((nom/hash y x) (lookupo x rest t))))))

(declare proper-listo)

(defn eval-expo [closure-nom exp env val]
  (conde
    [(fresh (v)
       (== `(~'quote ~v) exp)
       (nom/hash closure-nom v)
       (== v val))]
    [(fresh (as)
       (conso 'list as exp)
       (nom/hash closure-nom as)
       (proper-listo closure-nom as env val))]
    [(lookupo exp env val)]
    [(fresh (rator rand body envc a)
       (nom/fresh [x]
         (== `(~rator ~rand) exp)
         (eval-expo closure-nom rator env [closure-nom (nom/tie x body) envc])
         (eval-expo closure-nom rand env a)
         (eval-expo closure-nom body (lcons [x a] envc) val)))]
    [(fresh (body)
       (nom/fresh [x]
         (== (nom/tie x body) exp)
         (== [closure-nom (nom/tie x body) env] val)))]))

(defn proper-listo [closure-nom exp env val]
  (conde
    [(== '() exp)
     (== '() val)]
    [(fresh (a d t-a t-d)
       (conso a d exp)
       (conso t-a t-d val)
       (eval-expo closure-nom a env t-a)
       (proper-listo closure-nom d env t-d))]))

(defn cljo [e out]
  (conda
    [(lvaro e) (nom/fresh [x] (== out `(~'quote ~x)))]
    [(fresh [body bodyr]
       (nom/fresh [x]
         (== e (nom/tie x body))
         (== out `(~'fn [~x] ~bodyr))
         (cljo body bodyr)))]
    [(fresh [a d ar dr]
       (conso a d e)
       (conso ar dr out)
       (cljo a ar)
       (cljo d dr))]
    [(== e out)]))
