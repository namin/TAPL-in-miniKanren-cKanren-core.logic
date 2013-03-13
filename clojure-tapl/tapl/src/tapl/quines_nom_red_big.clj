(ns tapl.quines_nom_red_big
  (:use [tapl.utils])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp])
  (:import [java.io Writer]))

(declare els-substo)

(defn substo [e new a out]
  (conde
    [(nomo e) (== a e) (== new out)]
    [(nomo e)
      (== e out)
      (!= a e)]
    [(fresh [v]
       (== `(~'quote ~v) e)
       (== `(~'quote ~v) out))]
    [(fresh [body bodyres]
       (nom/fresh [c]
         (== `(~'fn ~(nom/tie c body)) e)
         (== `(~'fn ~(nom/tie c bodyres)) out)
         (nom/hash c a)
         (nom/hash c new)
         (substo body new a bodyres)))]
    [(fresh [els elsres]
       (conso 'list els e)
       (conso 'list elsres out)
       (els-substo els new a elsres))]
    [(fresh [rator ratorres rand randres]
       (== `(~rator ~rand) e)
       (== `(~ratorres ~randres) out)
       (substo rator new a ratorres)
       (substo rand new a randres))]))

(defn els-substo [els new a elsres]
  (conde
    [(== '() els)
     (== '() elsres)]
    [(fresh [first rest firstres restres]
       (conso first rest els)
       (conso firstres restres elsres)
       (substo first new a firstres)
       (els-substo rest new a restres))]))

(declare els-evalo)

(defn evalo [exp val]
  (conde
    [(fresh [v]
       (== `(~'quote ~v) exp)
       (== v val))]
    [(fresh [body]
       (nom/fresh [a]
         (== `(~'fn ~(nom/tie a body)) exp)
         (== exp val)))]
    [(fresh [as asres]
       (conso 'list as exp)
       (els-evalo as val))]
    [(fresh [rator rand body randval vali]
       (nom/fresh [a]
         (== `(~rator ~rand) exp)
         (evalo rator `(~'fn ~(nom/tie a body)))
         (evalo rand randval)
         (substo body randval a vali)
         (evalo vali val)))]))

(defn els-evalo [els val]
  (conde
    [(== '() els)
     (== '() val)]
    [(fresh [first rest firstval restval]
       (conso first rest els)
       (conso firstval restval val)
       (evalo first firstval)
       (els-evalo rest restval))]))
