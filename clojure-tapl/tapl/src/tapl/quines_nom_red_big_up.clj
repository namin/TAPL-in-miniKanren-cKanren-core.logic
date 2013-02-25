(ns tapl.quines_nom_red_big_up
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp])
  (:import [java.io Writer]))

(declare list-substo)

(defn substo [e new a out]
  (conde
    [(== a e) (== new out)]
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
    [(fresh [es vs]
       (conso 'list es e)
       (conso 'list vs out)
       (list-substo es new a vs))]
    [(fresh [rator ratorres rand randres]
       (== `(~rator ~rand) e)
       (!= rator 'quote)
       (!= rator 'fn)
       (!= rator 'list)
       (== `(~ratorres ~randres) out)
       (substo rator new a ratorres)
       (substo rand new a randres))]))

(defn list-substo [es new a out]
  (conde
    [(== es ()) (== out ())]
    [(fresh [first rest firstv restv]
       (conso first rest es)
       (conso firstv restv out)
       (substo first new a firstv)
       (list-substo rest new a restv))]))

(declare evalo)
(declare list-esubsto)

(defn esubsto [e new a out]
  (conde
    [(== a e) (== new out)]
    [(fresh [v]
       (== `(~'quote ~v) e)
       (== `(~'quote ~v) out))]
    [(fresh [es vs]
       (conso 'list es e)
       (== `(~'quote ~vs) out)
       (list-esubsto es new a vs))]
    [(fresh [rator ratorres rand randres i]
       (== `(~rator ~rand) e)
       (!= rator 'quote)
       (!= rator 'fn)
       (!= rator 'list)
       (== `(~ratorres ~randres) i)
       (substo rator new a ratorres)
       (substo rand new a randres)
       (evalo i out))]))

(defn list-esubsto [es new a vs]
  (conde
    [(== es ()) (== vs ())]
    [(fresh [first rest firstv restv]
       (conso first rest es)
       (conso firstv restv vs)
       (esubsto first new a `(~'quote ~firstv))
       (list-esubsto rest new a restv))]))

(defn not-tie? [x]
  (not (tie? x)))
(defn not-tie-o [x]
  (predc x not-tie? `not-tie?))

(declare list-evalo)

(defn evalo [exp val]
  (conde
    [(fresh [v]
       (== `(~'quote ~v) exp)
       (not-tie-o v)
       (== `(~'quote ~v) val))]
    [(fresh [body]
       (nom/fresh [a]
         (== `(~'fn ~(nom/tie a body)) exp)
         (== exp val)))]
    [(fresh [es vs]
       (conso 'list es exp)
       (== `(~'quote vs) val)
       (list-evalo es vs))]
    [(fresh [rator rand body randval]
       (nom/fresh [a]
         (== `(~rator ~rand) exp)
         (evalo rator `(~'fn ~(nom/tie a body)))
         (evalo rand randval)
         (esubsto body randval a val)))]))

(defn list-evalo [es vs]
  (conde
    [(== es ()) (== vs ())]
    [(fresh [first rest firstv restv]
       (conso first rest es)
       (conso firstv restv vs)
       (evalo first `(~'quote ~firstv))
       (list-evalo rest restv))]))
