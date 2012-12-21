(ns tapl.quines_nom_red_small
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]
        :reload)
  (:require [clojure.pprint :as pp])
  (:import [java.io Writer]))

(defn nomo [x]
  (predc x nom? `nom?))

(defn substo [e new a out]
  (conde
    [(== 'nil e) (== e out)]
    [(nomo e) (== a e) (== new out)]
    [(nomo e)
      (== e out)
      (nom/hash a e)]
    [(fresh [v]
       (== `(~'quote ~v) e)
       (== `(~'quote ~v) out))]
    [(fresh [first rest firstres restres]
       (== `(~'cons ~first ~rest) e)
       (== `(~'cons ~firstres ~restres) out)
       (substo first new a firstres)
       (substo rest new a restres))]
    [(fresh [body bodyres]
       (nom/fresh [c]
         (== `(~'fn ~(nom/tie c body)) e)
         (== `(~'fn ~(nom/tie c bodyres)) out)
         (nom/hash c a)
         (nom/hash c new)
         (substo body new a bodyres)))]
    [(fresh [rator ratorres rand randres]
       (== `(~rator ~rand) e)
       (== `(~ratorres ~randres) out)
       (substo rator new a ratorres)
       (substo rand new a randres))]))

(defn valo [exp]
  (conde
    [(== exp 'nil)]
    [(fresh [body]
       (nom/fresh [x]
         (== exp `(~'fn ~(nom/tie x body)))))]
    [(fresh [v]
       (== exp `(~'quote ~v)))]))

(defn listo [v]
  (conde
    [(== v nil)]
    [(fresh [a d]
       (conso a d v)
       (listo d))]))

(defn valofo [exp v]
  (conde
    [(== exp 'nil) (== v exp)]
    [(fresh [body]
       (nom/fresh [x]
         (== exp `(~'fn ~(nom/tie x body)))
         (== v exp)))]
    [(== exp `(~'quote ~v))]))

(defn redo [exp out]
  (conde
    [(fresh [first firstres rest]
       (== `(~'cons ~first ~rest) exp)
       (== `(~'cons ~firstres ~rest) out)
       (redo first firstres))]
    [(fresh [first rest restres]
       (== `(~'cons ~first ~rest) exp)
       (== `(~'cons ~first ~restres) out)
       (valo first)
       (redo rest restres))]
    [(fresh [first firstv rest restv v]
       (== `(~'cons ~first ~rest) exp)
       (valo first)
       (valo rest)
       (== rest 'nil)
       (valofo first firstv)
       (conso firstv nil v)
       (== `(~'quote ~v) out))]
    [(fresh [first firstv rest restv v]
       (== `(~'cons ~first ~rest) exp)
       (valo first)
       (valo rest)
       (== rest `(~'quote ~restv))
       (listo restv)
       (valofo first firstv)
       (conso firstv restv v)
       (== `(~'quote ~v) out))]
    [(fresh [rator rand body]
       (nom/fresh [a]
         (== `(~rator ~rand) exp)
         (== rator `(~'fn ~(nom/tie a body)))
         (valo rand)
         (substo body rand a out)))]
    [(fresh [rator rand ratorval]
       (nom/fresh [x]
         (== `(~rator ~rand) exp)
         (== `(~ratorval ~rand) out)
         (redo rator ratorval)))]
    [(fresh [rator rand randval]
       (nom/fresh [x]
         (== `(~rator ~rand) exp)
         (== `(~rator ~randval) out)
         (valo rator)
         (redo rand randval)))]))

(defn redo* [e1 e2]
  (conde
    [(redo e1 e2)]
    [(fresh [ep]
       (redo e1 ep)
       (redo* ep e2))]))
