(ns tapl.stlc_deriv
  (:use [tapl.stlc])
  (:use [logically.art.interpreters.meta])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp]))

(defn typingo-clause [head tail]
  (fresh [case gamma t ty]
    (== head ['typingo case gamma t ty])
    (conde
      [(== case 'T-Var)
       (nomo t) (lookupo gamma t ty)
       (== tail ())]
      [(== case 'T-Abs)
       (fresh [casex gammax ty1 ty2 t2]
         (nom/fresh [x]
           (== t `(~'fn ~(nom/tie x t2)))
           (== ty [:=> ty1 ty2])
           (conso [x ty1] gamma gammax)
           (== tail [['typingo casex gammax t2 ty2]])))]
      [(== case 'T-App)
       (fresh [case1 case2 ty11 ty12 t1 t2]
         (== t `(~t1 ~t2))
         (== ty ty12)
         (== tail [['typingo case2 gamma t2 ty11]
                   ['typingo case1 gamma t1 [:=> ty11 ty12]]]))])))

(def typingo-deriv (solve-proof-for typingo-clause))

