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
         (!= t1 'fn)
         (== ty ty12)
         (== tail [['typingo case1 gamma t1 [:=> ty11 ty12]]
                   ['typingo case2 gamma t2 ty11]]))])))

(def typingo-deriv (solve-proof-for typingo-clause))

(defn typingo-debug-clause [head tail]
  (conde
    [(fresh [x y]
       (== head ['== x y])
       (== x y)
       (== tail ()))]
    [(fresh [gamma t ty]
       (== head ['lookupo gamma t ty])
       (lookupo gamma t ty)
       (== tail ()))]
    [(fresh [case gamma t ty]
       (== head ['typingo case gamma t ty])
       (conde
         [(== case 'T-Var)
          (nomo t)
          (== tail [['lookupo gamma t ty]])]
         [(== case 'T-Abs)
          (fresh [casex gammax ty1 ty2 t2]
            (nom/fresh [x]
              (== t `(~'fn ~(nom/tie x t2)))
              (conso [x ty1] gamma gammax)
              (== tail [['typingo casex gammax t2 ty2]
                        ['== ty [:=> ty1 ty2]]])))]
         [(== case 'T-App)
           (fresh [case1 case2 ty1 ty2 ty11 ty12 t1 t2]
             (== t `(~t1 ~t2))
             (!= t1 'fn)
             (== tail [['typingo case1 gamma t1 ty1]
                       ['typingo case2 gamma t2 ty2]
                       ['== ty1 [:=> ty11 ty12]]
                       ['== ty2 ty11]
                       ['== ty ty12]]))]))]))

;; Adapted from logically.art.interpreters.meta/solve-proof-for.
(defn debug-proof-for [clause]
  (letfn [(solve0 [goal tree ok]
            (all
              (solve [goal] tree ok)
              (conda
                [(== ok true)]
                [(== ok false)])))
          (solve [goals tree ok]
            (conde
              [(== goals ())
               (== tree ())]
              [(fresh [g gs ts b tb ehead etail]
                 (conso g gs goals)
                 (conda
                   [(clause g b)
                    (conso [g '<-- tb] ts tree)
                    (solve b tb ok)]
                   [(conso [g 'error] ts tree)
                    (== ok false)])
                 (solve gs ts ok))]))]
    solve0))

(def typingo-only-debug (debug-proof-for typingo-clause))
(def typingo-debug (debug-proof-for typingo-debug-clause))
